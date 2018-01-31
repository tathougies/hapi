module Web.Hapi.Plugin.Manager where

import           Prelude hiding (lookup)

import           Web.Hapi.Config
import           Web.Hapi.File
import           Web.Hapi.Plugin.Idl hiding (Parser)
import           Web.Hapi.Plugin.Monad
import           Web.Hapi.Registry
import qualified Web.Hapi.Schema as Db
import           Web.Hapi.Types

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Error.Util ((??))
import           Control.Exception (Exception, throwIO, bracket)
import           Control.Lens hiding ((:>), (??))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Free.Church

import           Data.Aeson ( ToJSON(..), FromJSON(..)
                            , encode, decodeStrict
                            , genericToJSON, genericParseJSON
                            , defaultOptions )
import           Data.Aeson.Internal (IResult(..), ifromJSON)
import           Data.Aeson.Parser (value')
import           Data.Aeson.Types (Options(..), camelTo2)
import           Data.Attoparsec.ByteString hiding (Fail)
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (toLazyByteString, byteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Streaming.Char8 as S
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)
import           Data.Int
import           Data.List (find)
import           Data.Maybe
import qualified Data.Pool as P
import           Data.String
import           Data.Text (Text)
import           Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.UUID (UUID)
import           Data.Word

import           Database.Beam
import           Database.Beam.Postgres hiding (runInsert, insert)
import qualified Database.Beam.Postgres as Pg
import qualified Database.Memcache.Client as Memcache

import           Debug.Trace

import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

import           Network.BSD
import           Network.URI hiding (path)

import           Streaming

import           System.IO
import           System.Process

import qualified Web.JWT as JWT

data PluginError
  = PluginError ApiName Text
  | PluginAsync Int AsyncToken TypeName String
  deriving (Show, Generic)
instance ToJSON PluginError
instance FromJSON PluginError

data NoSuchPlugin = NoSuchPlugin ApiName
  deriving (Show, Eq, Ord)
instance Exception NoSuchPlugin

newtype PluginClientId = PluginClientId Word64
  deriving (Show, Eq, Ord, Enum, Hashable, Storable, Bounded)
data PluginWaitingClient where
  PluginWaitingStream :: (S.ByteString IO () -> IO (S.ByteString IO ())) -> PluginWaitingClient
  PluginWaitingOn     :: PluginData a => TMVar a -> PluginWaitingClient
  PluginWaitingQueue  :: PluginData a => TQueue a -> PluginWaitingClient

type NextAsync a = IO (Either PluginError (AsyncResponse a))

data AsyncResponse a
  = AsyncResponseContinues Int AsyncToken TypeName String (NextAsync a)
  | AsyncResponseDone a

data PluginManager
  = PluginManager
  { _pluginManagerActivePlugins :: TMVar (HM.HashMap ApiName PluginState)
  , _pluginManagerRegistry      :: HapiPluginRegistry
  , _pluginManagerConfig        :: HapiConfig
  , _pluginManagerEndpointLink  :: Text -> Text
  , _pluginManagerAsyncContinueLink :: AsyncToken -> Text
  , _pluginManagerDb            :: forall a. Pg a -> IO a
  , _pluginManagerFileBackend   :: HapiFileBackend
  , _pluginManagerMemcachePool  :: P.Pool Memcache.Client
  , _pluginManagerAsyncs        :: TVar (HM.HashMap AsyncToken (POSIXTime, TMVar HapiObject, TMVar HapiObject, TMVar (NextAsync (PluginType, HapiValue))))
  }

data PluginState
  = PluginState
  { _pluginStateNextToken      :: TVar PluginClientId
  , _pluginStateLastUse        :: TVar UTCTime
  , _pluginStateStdIn          :: TMVar Handle
  , _pluginStateClientsWaiting :: TVar (HM.HashMap PluginClientId PluginWaitingClient)
  , _pluginStateApiDescription :: ApiDescription
  , _pluginStateInitInfo       :: TMVar PluginInitInfo
  , _pluginStateClient         :: TMVar PluginClientState
  }

data PluginClientState
  = PluginClientState
  { _pluginClientStateMemcachePool :: P.Pool Memcache.Client
  }

data Plugin
  = Plugin
  { _pluginName           :: Text
  , _pluginIdl            :: Text
  , _pluginDescription    :: ApiDescription
  , _pluginConfigTemplate :: PluginM HapiObject
  , _pluginConfigureRequest, _pluginConfigTypeName :: Identifier
  , _pluginNormalize      :: TypeName   -> HapiObject -> PluginM HapiObject
  , _pluginFill           :: Identifier -> HapiObject -> PluginM HapiObject
  , _pluginForm           :: Identifier -> HapiObject -> PluginM (Either ApiSettingsControl ApiSettingsDescription)
  , _pluginImpl           :: Identifier -> HapiObject -> PluginM (PluginType, HapiValue)
  }

data PluginInitInfo
  = PluginInitInfo
  { _pluginInitMemcache :: String
  } deriving (Show, Generic)

instance ToJSON PluginInitInfo where
  toJSON = genericToJSON
             defaultOptions { fieldLabelModifier =
                                camelTo2 '-' .
                                dropPrefix "_pluginInit" }
instance FromJSON PluginInitInfo where
  parseJSON = genericParseJSON
                defaultOptions { fieldLabelModifier =
                                   camelTo2 '-' .
                                   dropPrefix "_pluginInit" }

data PluginRequest a where
  PluginRequestInitialize    :: PluginInitInfo -> PluginRequest ()
  PluginRequestName          :: PluginRequest Text
  PluginRequestIdl           :: PluginRequest Text

  PluginRequestConfigTypeName :: PluginRequest Identifier
  PluginRequestConfigRequest  :: PluginRequest Identifier

  PluginRequestDescription   :: PluginRequest ApiDescription
  PluginRequestInitialConfig :: ScopeContext -> PluginRequest (Identifier, HapiObject)
  PluginRequestNormalize     :: ScopeContext -> Identifier -> HapiObject -> PluginRequest HapiObject
  PluginRequestFill          :: ScopeContext -> Identifier -> HapiObject -> PluginRequest HapiObject
  PluginRequestForm          :: ScopeContext -> Identifier -> HapiObject -> PluginRequest (Either ApiSettingsControl ApiSettingsDescription)

  PluginRequestPerform       :: ScopeContext -> ApiRequestName -> HapiObject -> PluginRequest (PluginType, HapiValue)
deriving instance Show (PluginRequest a)

data SomePluginRequest where
  SomePluginRequest :: PluginData a => PluginRequest a -> SomePluginRequest
deriving instance Show SomePluginRequest

data PluginMasterRequest a where
  PluginMasterRequestGetDefault :: ScopeContext -> TypeName -> PluginMasterRequest HapiValue
  PluginMasterRequestGetValue   :: ScopeContext -> ApiName -> ApiRequestName -> PluginMasterRequest (PluginType, HapiValue)
  PluginMasterRequestGetConfig  :: UUID -> UUID -> PluginMasterRequest HapiObject
  PluginMasterRequestMkRequest  :: ScopeContext -> ApiName -> ApiRequestName -> TypeName -> HapiObject -> PluginMasterRequest (PluginType, HapiValue)
  PluginMasterRequestNormalize  :: ScopeContext -> TypeName -> HapiObject -> PluginMasterRequest HapiObject
  PluginMasterRequestGetControlForType :: ScopeContext -> TypeName -> HapiObject -> PluginMasterRequest ApiSettingsControl
  PluginMasterRequestFileName :: UUID -> PluginMasterRequest (Maybe Text)
  PluginMasterRequestGetFileContents :: UUID -> PluginMasterRequest BS.ByteString
  PluginMasterRequestSignAsync :: Int -> AsyncToken -> PluginMasterRequest AsyncToken
  PluginMasterRequestWaitAsync :: Int -> TypeName -> AsyncToken -> PluginMasterRequest HapiObject
  PluginMasterRequestKickAsync :: AsyncToken -> TypeName -> HapiObject -> PluginMasterRequest Text
  PluginMasterRequestGetInLookup :: ScopeContext -> Text -> HapiObject -> PluginMasterRequest (Maybe HapiObject)
  PluginMasterRequestSetLookup :: ScopeContext -> Text -> HapiObject -> HapiObject -> PluginMasterRequest ()
deriving instance Show (PluginMasterRequest a)
data SomeMasterPluginRequest where
  SomeMasterPluginRequest :: PluginData a => PluginMasterRequest a -> SomeMasterPluginRequest
deriving instance Show SomeMasterPluginRequest

data NewPluginRequest a = NewPluginRequest PluginClientId a
  deriving (Show, Eq)

class PluginData a where
  pluginDataRead  :: MonadIO m => S.ByteString m () -> m (Maybe a, S.ByteString m ())
  pluginDataWrite :: MonadIO m => a -> S.ByteString m ()

makeLenses ''PluginState
makeLenses ''PluginInitInfo
makeLenses ''PluginClientState
makeLenses ''PluginManager
makeLenses ''Plugin

pluginWriteJSON :: (ToJSON a, Monad m) => a -> S.ByteString m ()
pluginWriteJSON = S.fromLazy . encode

pluginReadJSON  :: (FromJSON a, Monad m) => S.ByteString m () -> m (Maybe a, S.ByteString m ())
pluginReadJSON s = do
  (res, s') <- A.parse value' s
  case res of
    Left a -> do
      case ifromJSON a of
        IError {} -> pure (Nothing, s')
        ISuccess x -> pure (Just x, s')
    Right {} -> pure (Nothing, s')

instance PluginData Text where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData AsyncToken where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance (PluginData a, PluginData b) => PluginData (Either a b) where
  pluginDataWrite (Left e) = "L" >> pluginDataWrite e
  pluginDataWrite (Right e) = "R" >> pluginDataWrite e

  pluginDataRead s = do
    c :> s' <- S.head (S.splitAt 1 s)
    case c of
      Just 'L' -> do
        (x, s'') <- pluginDataRead s'
        case x of
          Nothing -> pure (Nothing, s'')
          Just x' -> pure (Just (Left x'), s'')
      Just 'R' -> do
        (x, s'') <- pluginDataRead s'
        case x of
          Nothing -> pure (Nothing, s'')
          Just x' -> pure (Just (Right x'), s'')
      _ -> pure (Nothing, s')
instance PluginData PluginError where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData a => PluginData (Maybe a) where
  pluginDataWrite Nothing = "N"
  pluginDataWrite (Just a) = "J" >> pluginDataWrite a

  pluginDataRead s = do
    c :> s' <- S.head (S.splitAt 1 s)
    case c of
      Just 'N' -> pure (Just Nothing, s')
      Just 'J' -> do
        (x, s'') <- pluginDataRead s'
        case x of
          Nothing -> pure (Nothing, s'')
          Just x' -> pure (Just (Just x'), s'')
      _ -> pure (Nothing, s')
instance PluginData BS.ByteString where
  pluginDataWrite b = writeStorable (fromIntegral (BS.length b) :: Int64) >>
                      S.fromStrict b
  pluginDataRead s = do
    (l, s') <- readStorable s
    res :> s'' <- trace ("Reading " ++ show l) $ S.toStrict (S.splitAt (fromIntegral (l :: Int64)) s')
    pure (Just res, s'')
instance PluginData () where
  pluginDataWrite () = pure ()
  pluginDataRead s = pure (Just (), s)
instance PluginData ApiDescription where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData ApiSettingsDescription where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData ApiSettingsControl where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData HapiObject where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData Identifier where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData HapiValue where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData TypeName where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData ApiName where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData ApiRequestName where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance PluginData PluginType where
  pluginDataWrite = pluginWriteJSON
  pluginDataRead  = pluginReadJSON
instance (PluginData a, PluginData b) => PluginData (a, b) where
  pluginDataWrite (a, b) = pluginDataWrite a >> pluginDataWrite b
  pluginDataRead bs = do
    (a, bs')  <- pluginDataRead bs
    (b, bs'') <- pluginDataRead bs'
    pure ((,) <$> a <*> b, bs'')
instance PluginData SomePluginRequest where
  pluginDataWrite (SomePluginRequest a) = encodePluginRequest a
  pluginDataRead bs = do
    (res, bs') <- A.parse pluginParser bs
    case res of
      Left   x -> pure (Just x, bs')
      Right {} -> pure (Nothing, bs')
instance PluginData SomeMasterPluginRequest where
  pluginDataWrite (SomeMasterPluginRequest a) = encodeMasterPluginRequest a
  pluginDataRead bs = do
    (res, bs') <- A.parse masterParser bs
    case res of
      Left   x -> pure (Just x, bs')
      Right {} -> pure (Nothing, bs')
instance PluginData a => PluginData (NewPluginRequest a) where
  pluginDataWrite (NewPluginRequest clientId req) = writeStorable clientId >> pluginDataWrite req
  pluginDataRead bs = do
    (newId, bs') <- readStorable bs
    (res, bs'') <- pluginDataRead bs'
    pure (NewPluginRequest newId <$> res, bs'')

encodePluginRequest :: Monad m => PluginRequest a -> S.ByteString m ()
encodePluginRequest (PluginRequestInitialize initInfo) =
  "INIT" >> S.fromLazy (encode initInfo)
encodePluginRequest PluginRequestName = "NAME"
encodePluginRequest PluginRequestIdl  = "IDL"
encodePluginRequest PluginRequestDescription = "DESC"
encodePluginRequest PluginRequestConfigTypeName = "CNFNAME"
encodePluginRequest PluginRequestConfigRequest = "CNFREQ"
encodePluginRequest (PluginRequestInitialConfig scp) =
  "CONF" >> S.fromLazy (encode scp)
encodePluginRequest (PluginRequestNormalize scp ty obj) =
  "NORMALIZE" >> S.fromLazy (encode (scp, ty, obj))
encodePluginRequest (PluginRequestFill scp ty obj) =
  "FILL" >> S.fromLazy (encode (scp, ty, obj))
encodePluginRequest (PluginRequestForm scp ty obj) =
  "FORM" >> S.fromLazy (encode (scp, ty, obj))
encodePluginRequest (PluginRequestPerform scp reqNm obj) =
  "DO" >> S.fromLazy (encode (scp, reqNm, obj))

decodePluginRequest :: Monad m => S.ByteString m r -> Stream (Of SomePluginRequest) m (Either (A.Message, S.ByteString m r) r)
decodePluginRequest = A.parsed pluginParser

encodeMasterPluginRequest :: Monad m => PluginMasterRequest a -> S.ByteString m ()
encodeMasterPluginRequest (PluginMasterRequestGetDefault scp tyName) =
  "DEFAULT" >>
  S.fromLazy (encode (scp, tyName))
encodeMasterPluginRequest (PluginMasterRequestGetValue scp apiNm reqNm) =
  "GETVALUE" >>
  S.fromLazy (encode (scp, apiNm, reqNm))
encodeMasterPluginRequest (PluginMasterRequestGetConfig userUuid apiUuid) =
  "GETCONFIG" >>
  S.fromLazy (encode (userUuid, apiUuid))
encodeMasterPluginRequest (PluginMasterRequestMkRequest scp apiNm reqNm argTy arg) =
  "MKREQUEST" >>
  S.fromLazy (encode (scp, apiNm, reqNm, argTy, arg))
encodeMasterPluginRequest (PluginMasterRequestNormalize scp ty obj) =
  "NORMALIZE" >>
  S.fromLazy (encode (scp, ty, obj))
encodeMasterPluginRequest (PluginMasterRequestGetControlForType scp ty obj) =
  "CONTROL" >>
  S.fromLazy (encode (scp, ty, obj))
encodeMasterPluginRequest (PluginMasterRequestFileName fileId) =
  "FILENAME" >>
  S.fromLazy (encode fileId)
encodeMasterPluginRequest (PluginMasterRequestGetFileContents fileId) =
  "READFILE" >>
  S.fromLazy (encode fileId)
encodeMasterPluginRequest (PluginMasterRequestSignAsync timeout async) =
  "SIGNASYNC" >>
  S.fromLazy (encode (timeout, async))
encodeMasterPluginRequest (PluginMasterRequestWaitAsync timeout tyName async) =
  "WAITASYNC" >>
  S.fromLazy (encode (timeout, tyName, async))
encodeMasterPluginRequest (PluginMasterRequestKickAsync token tyName obj) =
  "KICKASYNC" >>
  S.fromLazy (encode (token, tyName, obj))
encodeMasterPluginRequest (PluginMasterRequestGetInLookup scp lookupNm key) =
  "LOOKUPGET" >>
  S.fromLazy (encode (scp, lookupNm, key))
encodeMasterPluginRequest (PluginMasterRequestSetLookup scp lookupNm key val) =
  "LOOKUPSET" >>
  S.fromLazy (encode (scp, lookupNm, key, val))

masterParser :: Parser SomeMasterPluginRequest
masterParser = SomeMasterPluginRequest <$> decodeGetDefault <|>
               SomeMasterPluginRequest <$> decodeGetValue   <|>
               SomeMasterPluginRequest <$> decodeGetConfig  <|>
               SomeMasterPluginRequest <$> decodeMkRequest  <|>
               SomeMasterPluginRequest <$> decodeNormalize  <|>
               SomeMasterPluginRequest <$> decodeGetControl <|>
               SomeMasterPluginRequest <$> decodeFileName   <|>
               SomeMasterPluginRequest <$> decodeReadFile   <|>
               SomeMasterPluginRequest <$> decodeSignAsync  <|>
               SomeMasterPluginRequest <$> decodeWaitAsync  <|>
               SomeMasterPluginRequest <$> decodeKickAsync  <|>
               SomeMasterPluginRequest <$> decodeLookupGet  <|>
               SomeMasterPluginRequest <$> decodeLookupSet  <?>
               "master request"
  where
    decodeGetDefault = do
      _ <- string "DEFAULT"
      ISuccess (scp, ty) <- ifromJSON <$> value'
      pure (PluginMasterRequestGetDefault scp ty)
    decodeGetValue = do
      _ <- try $ string "GETVALUE"
      ISuccess (scp, apiNm, reqNm) <- ifromJSON <$> value'
      pure (PluginMasterRequestGetValue scp apiNm reqNm)
    decodeGetConfig = do
      _ <- try $ string "GETCONFIG"
      ISuccess (userUuid, apiUuid) <- ifromJSON <$> value'
      pure (PluginMasterRequestGetConfig userUuid apiUuid)
    decodeMkRequest = do
      _ <- string "MKREQUEST"
      ISuccess (scp, apiNm, reqNm, argTy, arg) <- ifromJSON <$> value'
      pure (PluginMasterRequestMkRequest scp apiNm reqNm argTy arg)
    decodeNormalize = do
      _ <- string "NORMALIZE"
      ISuccess (scp, ty, obj) <- ifromJSON <$> value'
      pure (PluginMasterRequestNormalize scp ty obj)
    decodeGetControl = do
      _ <- string "CONTROL"
      ISuccess (scp, ty, obj) <- ifromJSON <$> value'
      pure (PluginMasterRequestGetControlForType scp ty obj)
    decodeFileName = do
      _ <- string "FILENAME"
      ISuccess fileId <- ifromJSON <$> value'
      pure (PluginMasterRequestFileName fileId)
    decodeReadFile = do
      _ <- string "READFILE"
      ISuccess fileId <- ifromJSON <$> value'
      pure (PluginMasterRequestGetFileContents fileId)
    decodeSignAsync = do
      _ <- string "SIGNASYNC"
      ISuccess (timeout, asyncToken) <- ifromJSON <$> value'
      pure (PluginMasterRequestSignAsync timeout asyncToken)
    decodeWaitAsync = do
      _ <- string "WAITASYNC"
      ISuccess (timeout, tyName, asyncToken) <- ifromJSON <$> value'
      pure (PluginMasterRequestWaitAsync timeout tyName asyncToken)
    decodeKickAsync = do
      _ <- string "KICKASYNC"
      ISuccess (async, tyName, obj) <- ifromJSON <$> value'
      pure (PluginMasterRequestKickAsync async tyName obj)
    decodeLookupGet = do
      _ <- try $ string "LOOKUPGET"
      ISuccess (scp, nm, key) <- ifromJSON <$> value'
      pure (PluginMasterRequestGetInLookup scp nm key)
    decodeLookupSet = do
      _ <- try $ string "LOOKUPSET"
      ISuccess (scp, nm, key, val) <- ifromJSON <$> value'
      pure (PluginMasterRequestSetLookup scp nm key val)

pluginParser :: Parser SomePluginRequest
pluginParser = (SomePluginRequest PluginRequestName <$ string "NAME") <|>
               (SomePluginRequest PluginRequestIdl  <$ string "IDL")  <|>
               (SomePluginRequest PluginRequestDescription <$ string "DESC") <|>
               (SomePluginRequest PluginRequestConfigTypeName <$ string "CNFNAME") <|>
               (SomePluginRequest PluginRequestConfigRequest <$ string "CNFREQ") <|>
               (SomePluginRequest <$> decodeInit) <|>
               (SomePluginRequest <$> decodeConf) <|>
               (SomePluginRequest <$> decodeFill)      <|>
               (SomePluginRequest <$> decodeNormalize) <|>
               (SomePluginRequest <$> decodeForm)      <|>
               (SomePluginRequest <$> decodePerform)   <?>
               "plugin request"
  where
    decodeInit = do
      _ <- string "INIT"
      ISuccess initInfo <- ifromJSON <$> value'
      pure (PluginRequestInitialize initInfo)
    decodeConf = do
      _ <- string "CONF"
      ISuccess scp <- ifromJSON <$> value'
      pure (PluginRequestInitialConfig scp)
    decodeFill = do
      _ <- string "FILL"
      ISuccess (scp, ty, obj) <- ifromJSON <$> value'
      pure (PluginRequestFill scp ty obj)
    decodeNormalize = do
      _ <- string "NORMALIZE"
      ISuccess (scp, ty, obj) <- ifromJSON <$> value'
      pure (PluginRequestNormalize scp ty obj)
    decodeForm = do
      _ <- string "FORM"
      ISuccess (scp, ty, obj) <- ifromJSON <$> value'
      pure (PluginRequestForm scp ty obj)
    decodePerform = do
      _ <- string "DO"
      ISuccess (scp, reqNm, args) <- ifromJSON <$> value'
      pure (PluginRequestPerform scp reqNm args)

-- * Multithreaded manager

internalClient :: PluginClientId
internalClient = maxBound

newPluginManager :: HapiConfig -> HapiPluginRegistry
                 -> (Text -> Text)
                 -> (AsyncToken -> Text)
                 -> (forall a. Pg a -> IO a)
                 -> HapiFileBackend
                 -> IO PluginManager
newPluginManager cnf reg endpointLink continueLink withDb fileBackend = do
  activePlugins <- newTMVarIO mempty
  asyncs <- newTVarIO mempty

  let memcacheServerSettings = parseMemcacheUrl (cnf ^. hapiConfigMemcache)
  memcachePool <-
      P.createPool (Memcache.newClient [memcacheServerSettings] Memcache.def)
                   Memcache.quit
                   1 60 25

  _ <- forkIO $ do
       forever $ do
         threadDelay 60000000
         curTime <- getPOSIXTime
         atomically $
           modifyTVar' asyncs (HM.filter (\(expTm, _, _, _) -> expTm < curTime))

  pure $ PluginManager activePlugins reg cnf endpointLink continueLink withDb fileBackend memcachePool asyncs

requestFromPlugin :: PluginData a => PluginManager -> ApiName -> PluginRequest a -> IO (Either PluginError a)
requestFromPlugin mgr apiNm req =
  do pluginSt <- getPluginSt mgr apiNm
     (ret, curTok) <-
       atomically $ do
         ret <- newEmptyTMVar
         curTok <- readTVar (pluginSt ^. pluginStateNextToken)
         writeTVar (pluginSt ^. pluginStateNextToken) (succ curTok)
         modifyTVar (pluginSt ^. pluginStateClientsWaiting) (at curTok ?~ PluginWaitingOn ret)
         pure (ret, curTok)

     writePluginHandle pluginSt (writeStorable internalClient >>
                                 pluginDataWrite (NewPluginRequest curTok (SomePluginRequest req)))
     res <- atomically $ takeTMVar ret
     case res of
       Left PluginAsync {} -> pure (Left (PluginError "com.hapi" "Unexpected async"))
       Left e -> pure (Left e)
       Right a -> pure (Right a)

requestFromPluginAsync :: PluginData a => PluginManager -> ApiName -> PluginRequest a -> IO (Either PluginError (AsyncResponse a))
requestFromPluginAsync mgr apiNm req =
  do pluginSt <- getPluginSt mgr apiNm

     (ret, curTok) <-
       atomically $ do
         ret <- newTQueue
         curTok <- readTVar (pluginSt ^. pluginStateNextToken)
         writeTVar (pluginSt ^. pluginStateNextToken) (succ curTok)
         modifyTVar (pluginSt ^. pluginStateClientsWaiting) (at curTok ?~ PluginWaitingQueue ret)
         pure (ret, curTok)

     writePluginHandle pluginSt (writeStorable internalClient >>
                                 pluginDataWrite (NewPluginRequest curTok (SomePluginRequest req)))

     let getNextResponse = do
           res <- atomically $ do
                    res <- readTQueue ret
                    case res of
                      Right {} -> modifyTVar (pluginSt ^. pluginStateClientsWaiting) (at curTok .~ Nothing)
                      _ -> pure ()
                    pure res
           case res of
             Left (PluginAsync timeout async tyName url) -> pure (Right (AsyncResponseContinues timeout async tyName url getNextResponse))
             Left e -> pure (Left e)
             Right a -> pure (Right (AsyncResponseDone a))

     getNextResponse

requestFromMaster :: PluginData a => PluginState -> PluginMasterRequest a -> IO (Either PluginError a)
requestFromMaster st req =
  do (ret, curTok) <-
       atomically $ do
         ret <- newEmptyTMVar
         curTok <- readTVar (st ^. pluginStateNextToken)
         writeTVar (st ^. pluginStateNextToken) (succ curTok)
         modifyTVar (st ^. pluginStateClientsWaiting) (at curTok ?~ PluginWaitingOn ret)
         pure (ret, curTok)

     writePluginHandle st (writeStorable internalClient >>
                           pluginDataWrite (NewPluginRequest curTok (SomeMasterPluginRequest req)))

     res <- atomically $ takeTMVar ret
     case res of
       Left err -> pure (Left (PluginError "com.hapi" err))
       Right x  -> pure (Right x)

getPluginSt :: PluginManager -> ApiName -> IO PluginState
getPluginSt mgr apiNm
  | Just plugin <- mgr ^. pluginManagerRegistry . registryApis . at apiNm =
      do res <-
           atomically $ do
             plugins <- readTMVar (mgr ^. pluginManagerActivePlugins)
             case plugins ^. at apiNm of
               Nothing -> Left <$> takeTMVar (mgr ^. pluginManagerActivePlugins)
               Just st -> pure $ Right st

         case res of
           Left curPlugins -> do
             let createProc = (proc (plugin ^. pluginExecutable) ["pluginserve"]) { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
             (Just childStdIn, Just childStdOut, _, _) <-
               createProcess createProc

             let initInfo = PluginInitInfo (mgr ^. pluginManagerConfig . hapiConfigMemcache)

             tm <- getCurrentTime
             (st, q) <- atomically $ do
                          q  <- newTQueue
                          st <- PluginState <$> newTVar minBound
                                            <*> newTVar tm
                                            <*> newTMVar childStdIn
                                            <*> newTVar (HM.singleton internalClient (PluginWaitingQueue q))
                                            <*> pure (plugin ^. pluginRegistrationDescription)
                                            <*> newTMVar initInfo
                                            <*> newEmptyTMVar
                          putTMVar (mgr ^. pluginManagerActivePlugins) (curPlugins & at apiNm ?~ st)
                          pure (st, q)

             _ <- forkIO . forever $ do
                    msg <- atomically $ readTQueue q
                    handleMessage st msg

             _ <- forkIO (pluginInputWorker childStdOut st)

             _ <- forkIO . void $ requestFromPlugin mgr apiNm (PluginRequestInitialize initInfo)

             pure st
           Right st ->
             pure st
  | otherwise = throwIO (NoSuchPlugin apiNm)
  where
    handleMessage st (NewPluginRequest newId (SomeMasterPluginRequest req)) = do
      hPutStrLn stderr ("Master plugin " ++ show req)
      _ <- forkIO $ do
             rsp <- respondToMasterRequest mgr apiNm req
             writePluginHandle st (writeStorable newId >>
                                   pluginDataWrite rsp)
      pure ()

verifyPluginPublicAccess :: ApiDescription -> ScopeContext -> ApiRequestName -> Maybe ScopeContext
verifyPluginPublicAccess descr ctxt reqNm =
  listToMaybe $ do
    req <- descr ^. apiDescriptionRequests
    guard (req ^. apiRequestVisible == ApiVisibilityPublic && req ^. apiRequestName == reqNm)
    Just newCtxt <- pure (ctxt ^? atScope (req ^. apiRequestScope))
    pure newCtxt

getPublicEndpoint :: PluginManager -> ApiName -> ScopeContext -> ApiRequestName -> IO (Maybe Text)
getPublicEndpoint mgr apiNm ctxt req = do
  plugins <- atomically $ readTMVar (mgr ^. pluginManagerActivePlugins)
  case plugins ^. at apiNm of
    Nothing -> pure Nothing
    Just plugin -> do
      let api = plugin ^. pluginStateApiDescription

      case verifyPluginPublicAccess api ctxt req of
        Nothing -> pure Nothing
        Just ctxt' ->
          _pluginManagerDb mgr $ do
            let userKey = Db.UserKey userKey'
                userKey' = ctxt' ^? scopeContextUser . userIdUuid
                apiKey  = Db.ApiRegistrationKey apiKey'
                apiKey' = ctxt' ^? scopeContextApi . apiIdUuid

            endpoint <- runSelectReturningOne $
                        select $
                        filter_ (\e -> maybe (val_ True)
                                             (\userKey'' -> e ^. Db.endpointUser ==. Db.UserKey (val_ (Just userKey'')))
                                             userKey' &&.
                                       maybe (val_ True)
                                             (\apiKey'' -> e ^. Db.endpointApi  ==. Db.ApiRegistrationKey (val_ (Just apiKey'')))
                                             apiKey' &&.
                                       e ^. Db.endpointRequest ==. val_ (req ^. _ApiRequestName)) $
                        all_ (Db.hapiDb ^. Db.dbPublicEndpoints)
            case endpoint of
              Nothing -> do
                pathNum <- liftIO randomString
                let path = "endpoint-" <> pathNum <> "-" <> (apiNm ^. _ApiName) <> "-" <> (req ^. _ApiRequestName)
                runInsert $ insert (Db.hapiDb ^. Db.dbPublicEndpoints) $
                  insertValues [ Db.PublicEndpoint path userKey apiKey (apiNm ^. _ApiName) (req ^. _ApiRequestName) ]
                pure (Just ((mgr ^. pluginManagerEndpointLink) path))
              Just endpoint' ->
                pure (Just ((mgr ^. pluginManagerEndpointLink) (endpoint' ^. Db.endpointPath)))

decodeAsyncToken :: MonadIO m => HapiConfig -> Maybe ApiName -> AsyncToken -> ExceptT Text m (ApiName, AsyncToken)
decodeAsyncToken cfg apiNm (AsyncToken signedToken) =
  case JWT.decode signedToken of
    Nothing -> throwError "Invalid async token (bad form)"
    Just signedToken' ->
      case JWT.verify (JWT.secret (cfg ^. hapiConfigJwtSecret)) signedToken' of
        Nothing -> throwError "Invalid async token (bad signature)"
        Just verified -> do
          curTime <- liftIO getPOSIXTime
          let claims = JWT.claims verified

          actualToken <- JWT.stringOrURIToText <$> JWT.sub claims ?? "Could not decode async token"
          issuedApiNm <- ApiName . JWT.stringOrURIToText <$> JWT.iss claims ?? "Could not verify async token source"
          expTime <- JWT.secondsSinceEpoch <$> JWT.exp claims ?? "Async token has no expiration"
          if maybe False (\apiNm' -> issuedApiNm /= apiNm') apiNm
            then throwError "Invalid async token (wrong issuer)"
            else if curTime > expTime
              then throwError "Expired async token"
              else pure (issuedApiNm, AsyncToken actualToken)

findLookup :: PluginManager -> ApiName -> Text -> IO (Either Text ApiLookupDescription)
findLookup mgr apiNm lookupNm = do
  plugins <- atomically $ readTMVar (mgr ^. pluginManagerActivePlugins)
  case plugins ^? at apiNm . _Just . pluginStateApiDescription . apiDescriptionLookups of
    Nothing -> pure (Left "Invalid API")
    Just lookups ->
      case find (\l -> l ^. apiLookupName . _Identifier == lookupNm) lookups of
        Nothing -> pure (Left "Invalid lookup name")
        Just lk -> pure (Right lk)

respondToMasterRequest :: PluginManager -> ApiName -> PluginMasterRequest a -> IO (Either Text a)
respondToMasterRequest mgr (ApiName apiNm) (PluginMasterRequestGetConfig userUuid apiUuid) =
  _pluginManagerDb mgr $ do
    dbApi <- runSelectReturningOne $ select $
             fmap Db._apiData $
             filter_ (\api -> api ^. Db.apiId ==. val_ apiUuid &&.
                              api ^. Db.apiUser ==. val_ (Db.UserKey userUuid) &&.
                              api ^. Db.apiName ==. val_ apiNm) $
             all_ (Db.hapiDb ^. Db.dbApis)
    case dbApi of
      Nothing -> pure (Left "No such API")
      Just (PgJSONB apiData) -> pure (Right apiData)
respondToMasterRequest _ (ApiName apiNm) (PluginMasterRequestGetValue _ "com.hapi" "api_url") =
  pure (Right (PluginTypeBuiltin BuiltinTypeString, HapiValueText ("/api/" <> apiNm)))
respondToMasterRequest mgr _ (PluginMasterRequestGetValue ctxt "com.hapi" "app_url")
  | Just user <- ctxt ^? scopeContextUser
  , Just userUuid <- user ^? userIdUuid =
      _pluginManagerDb mgr $ do
        dbUser <- runSelectReturningOne $ lookup (Db.hapiDb ^. Db.dbUsers) (Db.UserKey userUuid)
        case dbUser of
          Nothing -> pure (Left "no app_url")
          Just dbUser' -> pure (Right (PluginTypeBuiltin BuiltinTypeString, HapiValueText (dbUser' ^. Db.userHomepage)))
respondToMasterRequest mgr apiNm (PluginMasterRequestMkRequest ctxt "com.hapi" "GetPublicUrl" (TypeName (Just "com.hapi") "GetPublicUrl") (HapiObject args))
  | Just requestNm <- args ^? at "requestNm" . _Just . _HapiValueText = do
      url <- getPublicEndpoint mgr apiNm ctxt (ApiRequestName requestNm)
      case url of
        Nothing -> pure (Left "No such request")
        Just url' -> pure (Right (PluginTypeBuiltin BuiltinTypeString, HapiValueText url'))
respondToMasterRequest _ (ApiName apiNm) (PluginMasterRequestGetValue _ "com.hapi" "api_authorization_url") =
  pure (Right (PluginTypeBuiltin BuiltinTypeString, HapiValueText ("/authorization/" <> apiNm)))
respondToMasterRequest mgr _ (PluginMasterRequestFileName fileId) =
  fmap Right $
  _pluginManagerDb mgr $
  fmap (fmap Db._fileUploadName) $ runSelectReturningOne $ lookup (Db.hapiDb ^. Db.dbUploads) (Db.FileUploadKey fileId)
respondToMasterRequest mgr _ (PluginMasterRequestGetFileContents fileId) = do
  res <- _pluginManagerDb mgr $
         fmap (fmap Db._fileUploadData) $
         runSelectReturningOne $
         lookup (Db.hapiDb ^. Db.dbUploads) (Db.FileUploadKey fileId)
  case res of
    Nothing -> pure (Left "No such file")
    Just (PgJSONB fileData) -> do
      nextChunk <- fileBackendRead (mgr ^. pluginManagerFileBackend) fileData
      let go a = do
            c <- nextChunk
            if BS.null c then pure (Right (BL.toStrict (toLazyByteString a)))
              else go (a <> byteString c)
      go mempty
respondToMasterRequest mgr apiNm (PluginMasterRequestSignAsync timeout (AsyncToken tokenData)) = do
  curTime <- getPOSIXTime
  let signedToken = JWT.encodeSigned JWT.HS256 (JWT.secret (mgr ^. pluginManagerConfig . hapiConfigJwtSecret)) claims
      expTime = curTime + fromIntegral timeout
      claims = def { JWT.iss = JWT.stringOrURI (apiNm ^. _ApiName)
                   , JWT.sub = JWT.stringOrURI tokenData
                   , JWT.exp = JWT.numericDate expTime }
  hPutStrLn stderr ("Saving async token " ++ show tokenData)
  -- Add the async token to the map
  atomically $ do
    asyncs <- readTVar (mgr ^. pluginManagerAsyncs)
    case asyncs ^. at (AsyncToken tokenData) of
      Just {} -> pure (Left "Duplicate async token")
      Nothing -> do
        condVar <- newEmptyTMVar
        reqVar <- newEmptyTMVar
        respVar <- newEmptyTMVar
        modifyTVar' (mgr ^. pluginManagerAsyncs) (at (AsyncToken tokenData) ?~ (expTime, condVar, reqVar, respVar))
        pure (Right (AsyncToken signedToken))
respondToMasterRequest mgr apiNm (PluginMasterRequestGetInLookup scp lookupNm key) =
  runExceptT $ do
  lk <- ExceptT $ findLookup mgr apiNm lookupNm
  case scp ^? atScope (lk ^. apiLookupScope) of
    Nothing -> throwError "Too general a scope to perform lookup"
    Just scp' -> do
      let key' = HapiLookupKey (scp' ^? scopeContextUser) (scp' ^? scopeContextApi) (toJSON key)
      res <-
        liftIO $ _pluginManagerDb mgr $ runSelectReturningOne $ select $
        fmap (\lk' -> lk' ^. Db.lookupEntryValue) $
        filter_ (\lk' -> lk' ^. Db.lookupEntryFullKey ==. val_ (PgJSONB key')) $
        all_ (Db.hapiDb ^. Db.dbLookups)
      case res of
        Nothing -> pure Nothing
        Just (PgJSONB obj) -> pure (Just obj)
respondToMasterRequest mgr apiNm (PluginMasterRequestSetLookup scp lookupNm key val) =
  runExceptT $ do
  lk <- ExceptT $ findLookup mgr apiNm lookupNm
  case scp ^? atScope (lk ^. apiLookupScope) of
    Nothing -> throwError "Too general a scope to set lookup"
    Just scp' -> do
      let key' = HapiLookupKey (scp' ^? scopeContextUser) (scp' ^? scopeContextApi) (toJSON key)
          dbUserKey = Db.UserKey (scp' ^? scopeContextUser . userIdUuid)
          dbRegistrationKey = Db.ApiRegistrationKey (scp' ^? scopeContextApi . apiIdUuid)
      liftIO $ _pluginManagerDb mgr $ runInsert $
        Pg.insert (Db.hapiDb ^. Db.dbLookups)
                  (insertValues [ Db.LookupEntry (PgJSONB key') dbUserKey dbRegistrationKey (PgJSONB val) ])
                  (onConflict (conflictingFields (\tbl-> tbl ^. Db.lookupEntryFullKey))
                                (onConflictUpdateInstead (\tbl -> tbl ^. Db.lookupEntryValue)))
      pure ()
respondToMasterRequest mgr apiNm (PluginMasterRequestWaitAsync timeout _ signedToken) = do
  res <- runExceptT $ decodeAsyncToken (mgr ^. pluginManagerConfig) (Just apiNm) signedToken
  case res of
    Left e -> pure (Left e)
    Right (_, AsyncToken actualToken) ->
      runExceptT $ do
        completeVar <-
          ExceptT . atomically $ do
          asyncs <- readTVar (mgr ^. pluginManagerAsyncs)
          case asyncs ^. at (AsyncToken actualToken) of
            Nothing -> pure (Left "Invalid async token (perhaps try another host)")
            Just (_, _, v, _) -> pure $ Right v

        expVar <- liftIO $ registerDelay (timeout * 1000000)
        let readAsync = Right <$> takeTMVar completeVar
            expired = do
              isExpired <- readTVar expVar
              if isExpired then pure (Left "WaitAsync expired") else retry
        ExceptT $ atomically (readAsync <|> expired)
respondToMasterRequest mgr apiNm (PluginMasterRequestKickAsync signedToken tyNm obj) = do
  hPutStrLn stderr "Kick async"
  res <- runExceptT $ decodeAsyncToken (mgr ^. pluginManagerConfig) (Just apiNm) signedToken
  case res of
    Left e -> pure (Left e)
    Right (_, actualToken) -> do
     hPutStrLn stderr ("Lookup 1 " ++ show actualToken)
     lookupAsyncInfo mgr actualToken Left $ \asyncInfo -> do
      ourName <- getHostName
      hPutStrLn stderr ("Lookup " ++ show actualToken)
      if asyncInfo ^. asyncInfoLocation == fromString ourName
        then if tyNm /= asyncInfo ^. asyncInfoExpType
             then pure (Left "Type mismatch when kicking async")
             else
               atomically $ do
                 asyncs <- readTVar (mgr ^. pluginManagerAsyncs)
                 case asyncs ^. at actualToken of
                   Nothing -> pure (Left "Inconsistent async state")
                   Just (_, v, _, _) -> do
                     putTMVar v obj
                     pure (Right ((mgr ^. pluginManagerAsyncContinueLink) signedToken))
        else pure (Left "Cannot kick remote async yet")
respondToMasterRequest mgr apiNm (PluginMasterRequestGetControlForType scp (TypeName Nothing tyNm) v) =
  respondToMasterRequest mgr apiNm (PluginMasterRequestGetControlForType scp (TypeName (Just (apiNm ^. _ApiName . re _PackageName)) tyNm) v)
respondToMasterRequest mgr _ (PluginMasterRequestGetControlForType scp fullTypeNm@(TypeName (Just tyApiNm) tyNm) v) = do
  e <- requestFromPlugin mgr (tyApiNm ^. _PackageName . re _ApiName) (PluginRequestForm scp tyNm v)
  case e of
    Left PluginAsync {} -> pure (Left "Unexpected Async")
    Left (PluginError _ msg) -> pure (Left msg)
    Right (Left c) -> pure (Right c)
    Right (Right d) ->
      pure (Right (ApiSettingsControlMulti fullTypeNm d))
-- respondToMasterRequest _ _ (PluginMasterRequestGetDefault _ ty@(TypeName (Just "com.hapi") "FileUpload")) =
--   pure (HapiValueObject ty (HapiObject (HM.fromList [ ("url", HapiValueNothing), ("is_uploaded", HapiValueSwitch False) ])))
-- respondToMasterRequest _ _ (PluginMasterRequestGetControlForType _ (TypeName (Just "com.hapi") "FileUpload") (HapiObject obj)) =
--   hPutStrLn stderr "Get file upload control" >>
--   case obj ^? at "is_uploaded" . _Just . _HapiValueSwitch of
--     Just True ->
--       case obj ^? at "url" . _Just . _HapiValueJust of
--         Just (HapiValueText url) ->
--           pure (ApiSettingsControlFileUpload (Just (url, url)))
--         _ -> pure (ApiSettingsControlFileUpload Nothing)
--     _ -> pure (ApiSettingsControlFileUpload Nothing)
respondToMasterRequest _ _ req = pure (Left (fromString ("unimplemented " ++ show req)))

lookupAsyncInfo :: PluginManager -> AsyncToken
                -> (Text -> a)
                -> (HapiAsyncInfo -> IO a)
                -> IO a
lookupAsyncInfo mgr signedToken onError action =
  P.withResource (mgr ^. pluginManagerMemcachePool) $ \client -> do
  let key = "com.hapi.async:" <> TE.encodeUtf8 (signedToken ^. _AsyncToken)
  val <- Memcache.get client key
  case val of
    Just (valBs, _, _)
      | Just asyncInfo <- decodeStrict valBs ->
          action asyncInfo
      | otherwise -> pure (onError "Cannot decode async record")
    Nothing -> pure (onError "async token not found")

writePluginHandle :: PluginState -> S.ByteString IO a -> IO a
writePluginHandle st go =
  bracket (atomically . takeTMVar . _pluginStateStdIn $ st)
          (atomically . putTMVar (_pluginStateStdIn st)) $ \hdl -> do
    r <- S.toHandle hdl go
    hFlush hdl
    pure r

readStorable :: forall a r m. (Storable a, MonadIO m) => S.ByteString m r -> m (a, S.ByteString m r)
readStorable b = do
  let b' = S.splitAt (fromIntegral (sizeOf (undefined :: a))) b
  dat :> b'' <- S.toStrict b'

  let (datPtr, datOff, datLength) = BS.toForeignPtr dat
  if datLength /= sizeOf (undefined :: a)
    then fail ("readStorable " ++ show datLength ++ " /= " ++ show (sizeOf (undefined :: a)))
    else do
      a <- liftIO .
           withForeignPtr datPtr $ \datPtr_ ->
           peek (castPtr (datPtr_ `plusPtr` datOff))
      pure (a, b'')

writeStorable :: forall a m. (Storable a, MonadIO m) => a -> S.ByteString m ()
writeStorable a =
  S.mwrap $ do
    b <- liftIO .
         BS.create (fromIntegral (sizeOf (undefined :: a))) $ \aPtr ->
           poke (castPtr aPtr) a
    pure (S.chunk b)

-- | Manages input from a plugin
pluginInputWorker :: Handle -> PluginState -> IO ()
pluginInputWorker pluginStdOut st =
  demux (S.fromHandle pluginStdOut)
  where
    demux bs = do
      (res, bs') <- readStorable bs

      waiting <-
        atomically $ do
          waiting <- readTVar (st ^. pluginStateClientsWaiting)
          case waiting ^. at res of
            Nothing -> pure Nothing
            Just on -> do
              case on of
                PluginWaitingQueue {} -> pure (Just on)
                _ -> writeTVar (st ^. pluginStateClientsWaiting) (waiting & at res .~ Nothing) >>
                     pure (Just on)

      case waiting of
        -- This is safe because plugins are trustworthy
        Nothing -> fail ("No such waiting request" ++ show res)
        Just on ->
          case on of
            PluginWaitingStream stream -> do
              bs'' <- stream bs'
              demux bs''
            PluginWaitingOn var -> do
              (a, bs'') <- pluginDataRead bs'
              case a of
                Nothing -> fail "Input worker failure: waiting on var"
                Just a' ->
                  atomically $ putTMVar var a'
              demux bs''
            PluginWaitingQueue queue -> do
              (a, bs'') <- pluginDataRead bs'
              case a of
                Nothing -> fail "Input worker failure: waiting on queue"
                Just a'  -> atomically $ writeTQueue queue a'
              demux bs''

-- * Client plugin

newQueueReader :: PluginData a => PluginState -> (a -> IO ()) -> IO ()
newQueueReader st onMsg = do
  q <- atomically $ do
         q <- newTQueue
         curTok <- readTVar (st ^. pluginStateNextToken)
         modifyTVar (st ^. pluginStateClientsWaiting) (HM.insert curTok (PluginWaitingQueue q))
         writeTVar (st ^. pluginStateNextToken) (succ curTok)
         pure q

  _ <- forkIO . forever $ do
         msg <- atomically $ readTQueue q
         onMsg msg
  pure ()

parseMemcacheUrl :: String -> Memcache.ServerSpec
parseMemcacheUrl url =
  case parseURI url of
    Just uri
      | uriScheme uri == "memcache:", Just auth <- uriAuthority uri ->
        let port = case uriPort auth of
                     ':':port' -> read port'
                     _ -> 11211
            auth' =
              case uriUserInfo auth of
                [] -> Memcache.NoAuth
                userInfo ->
                  case span (/= ':') (init userInfo) of
                    (user, ':':pw) -> Memcache.Auth (fromString user) (fromString pw)
                    _ -> error $ "Invalid username/password for memcache: " ++ userInfo
        in Memcache.ServerSpec (uriRegName auth) port auth'
    _ -> error $ "Invalid memcache url: " ++ url

clientPluginMain :: Plugin -> IO ()
clientPluginMain plugin =
  do q <- atomically newTQueue

     st <- PluginState <$> newTVarIO  minBound
                       <*> (newTVarIO =<< getCurrentTime)
                       <*> newTMVarIO stdout
                       <*> newTVarIO  (HM.singleton internalClient (PluginWaitingQueue q))
                       <*> pure (plugin ^. pluginDescription)
                       <*> newEmptyTMVarIO
                       <*> newEmptyTMVarIO

     _ <- forkIO . forever $ do
            msg <- atomically $ readTQueue q
            handleMessage st msg

     pluginInputWorker stdin st
  where
    handleMessage st (NewPluginRequest resp (SomePluginRequest req)) =
      doPluginRequest st (\x -> writePluginHandle st (writeStorable resp >> pluginDataWrite x)) req

    doPluginRequest :: PluginState -> (Either PluginError a -> IO ()) -> PluginRequest a -> IO ()
    doPluginRequest st resp (PluginRequestInitialize initInfo) = do
      let memcacheServerSettings = parseMemcacheUrl (initInfo ^. pluginInitMemcache)
      memcachePool <-
        P.createPool (Memcache.newClient [memcacheServerSettings] Memcache.def)
                     Memcache.quit
                     1 60 25

      atomically $ do
        putTMVar (st ^. pluginStateInitInfo) initInfo
        putTMVar (st ^. pluginStateClient) (PluginClientState memcachePool)

      resp (Right ())
    doPluginRequest _ resp PluginRequestName =
      resp (Right (plugin ^. pluginName))
    doPluginRequest _ resp PluginRequestConfigTypeName =
      resp (Right (plugin ^. pluginConfigTypeName))
    doPluginRequest _ resp PluginRequestConfigRequest =
      resp (Right (plugin ^. pluginConfigureRequest))
    doPluginRequest st resp (PluginRequestInitialConfig scp) = do
      _ <- forkIO $
        runPluginMStreaming st plugin scp (plugin ^. pluginConfigTemplate) $ \r ->
          resp ((plugin ^. pluginConfigTypeName,) <$> r)
      pure ()
    doPluginRequest st resp (PluginRequestNormalize scp tyIdentifier obj) = do
      _ <- forkIO $
        runPluginMStreaming st plugin scp ((plugin ^. pluginNormalize) (TypeName Nothing tyIdentifier) obj) resp
      pure ()
    doPluginRequest st resp (PluginRequestFill scp tyIdentifier obj) = do
      _ <- forkIO $
           runPluginMStreaming st plugin scp ((plugin ^. pluginFill) tyIdentifier obj) resp
      pure ()
    doPluginRequest st resp (PluginRequestForm scp tyIdentifier obj) = do
      _ <- forkIO $
           runPluginMStreaming st plugin scp ((plugin ^. pluginForm) tyIdentifier obj) resp
      pure ()
    doPluginRequest st resp (PluginRequestPerform scp reqNm args) = do
      _ <- forkIO $
           runPluginMStreaming st plugin scp ((plugin ^. pluginImpl) (reqNm ^. _ApiRequestName . re _Identifier) args) resp
      pure ()
    doPluginRequest _ resp _ = resp (Left (PluginError (plugin ^. pluginName . re _ApiName) "Unknown message"))

runPluginMStreaming :: PluginData a
                    => PluginState -> Plugin -> ScopeContext
                    -> PluginM a
                    -> (Either PluginError a -> IO ())
                    -> IO ()
runPluginMStreaming st plugin scp (PluginM action) finish =
  finish =<< runExceptT (runF action pure doStep)
  where
    doStep :: forall a. PluginF (ExceptT PluginError IO a) -> ExceptT PluginError IO a
    doStep (GetPluginName next) =
      next (st ^. pluginStateApiDescription . apiDescriptionName)
    doStep (GetPluginConfig next) =
      case scp of
        ScopeContextApi userId' apiId'
          | Just userUuid <- userId' ^? userIdUuid
          , Just apiUuid <- apiId' ^? apiIdUuid -> do
            obj <- ExceptT $ requestFromMaster st (PluginMasterRequestGetConfig userUuid apiUuid)

            let configTypeName = plugin ^. pluginConfigTypeName
                fullTypeName = TypeName (Just (PackageName (plugin ^. pluginName))) configTypeName

                PluginM doFill = (plugin ^. pluginFill) configTypeName obj

            runF doFill (next . HapiValueObject fullTypeName) doStep
        _ -> throwError (PluginError (plugin ^. pluginName . re _ApiName) "No config outside of API instance scope")
    doStep (MkRequest apiName reqNm Nothing next) = do
      (tyNm, val) <- ExceptT $ requestFromMaster st (PluginMasterRequestGetValue scp apiName reqNm)
      next tyNm val
    doStep (MkRequest apiName reqNm (Just (argTy, arg)) next) = do
      (tyNm, val) <- ExceptT $ requestFromMaster st (PluginMasterRequestMkRequest scp apiName reqNm argTy arg)
      next tyNm val
    doStep (GetDefault forType next) = do
      val <- ExceptT $ requestFromMaster st (PluginMasterRequestGetDefault scp forType)
      next val
    doStep (Normalize tyName obj next) = do
      val <- ExceptT $ requestFromMaster st (PluginMasterRequestNormalize scp tyName obj)
      next val
    doStep (GetControlForType tyName obj next) = do
      val <- ExceptT $ requestFromMaster st (PluginMasterRequestGetControlForType scp tyName obj)
      next val
    doStep (GetFileName fileId next) = do
      val <- ExceptT $ requestFromMaster st (PluginMasterRequestFileName fileId)
      next val
    doStep (GetCurrentTime next) =
      next =<< liftIO getCurrentTime
    doStep (ReadFile fileId next) = do
      val <- ExceptT $ requestFromMaster st (PluginMasterRequestGetFileContents fileId)
      next val
    doStep (DoIO x next) = liftIO x >>= next
    doStep (Fail x) = throwError (PluginError (plugin ^. pluginName . re _ApiName) (fromString x))
    doStep (GoAsync timeout tyName mkUri next) = do
      token  <- AsyncToken <$> liftIO randomString
      token' <- ExceptT $ requestFromMaster st (PluginMasterRequestSignAsync timeout token)
      liftIO $ finish (Left (PluginAsync timeout token tyName (mkUri token')))

      val <- ExceptT $ requestFromMaster st (PluginMasterRequestWaitAsync timeout tyName token')
      next val
    doStep (KickAsync token givenTy obj next) = do
      ret <- ExceptT $ requestFromMaster st (PluginMasterRequestKickAsync token givenTy obj)
      next ret
    doStep (WithMemcache go next) = do
      client <- liftIO . atomically $ readTMVar (st ^. pluginStateClient)
      res <-
        ExceptT . liftIO . P.withResource (client ^. pluginClientStateMemcachePool) $ \client' ->
        let PluginM go' = go client'
        in runExceptT $ runF go' pure doStep
      next res
    doStep (GetInLookup lookupNm key next) = do
      val <- ExceptT $ requestFromMaster st (PluginMasterRequestGetInLookup scp lookupNm key)
      next val
    doStep (SetLookup lookupNm key val next) = do
      ExceptT $ requestFromMaster st (PluginMasterRequestSetLookup scp lookupNm key val)
      next
