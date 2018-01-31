module Web.Hapi.Plugin.Monad where

import           Web.Hapi.Plugin.Idl
import           Web.Hapi.Types

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Free.Church
import           Control.Monad.IO.Class

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
import           Data.Text.Lens
import           Data.Time
import           Data.UUID (UUID)

import qualified Database.Memcache.Client as Memcache

import           Network.URI

newtype LookupName k v = LookupName Text
  deriving Show

data PluginF next where
  GetPluginName :: (ApiName -> next) -> PluginF next
  GetPluginConfig :: (HapiValue -> next) -> PluginF next

  MkRequest :: ApiName -> ApiRequestName -> Maybe (TypeName, HapiObject)
            -> (PluginType -> HapiValue -> next) -> PluginF next
  GetDefault :: TypeName -> (HapiValue -> next) -> PluginF next
  Normalize :: TypeName -> HapiObject -> (HapiObject -> next) -> PluginF next
  GetControlForType :: TypeName -> HapiObject -> (ApiSettingsControl -> next) -> PluginF next
  GetCurrentTime :: (UTCTime -> next) -> PluginF next

  GetFileName :: UUID -> (Maybe Text -> next) -> PluginF next
  ReadFile :: UUID -> (B.ByteString -> next) -> PluginF next

  DoIO :: IO a -> (a -> next) -> PluginF next
  Fail :: String -> PluginF next

  GoAsync :: Int -> TypeName -> (AsyncToken -> String) -> (HapiObject -> next) -> PluginF next
  KickAsync :: AsyncToken -> TypeName -> HapiObject -> (Text -> next) -> PluginF next

  GetInLookup :: Text -> HapiObject -> (Maybe HapiObject -> next) -> PluginF next
  SetLookup :: Text -> HapiObject -> HapiObject -> next -> PluginF next

  WithMemcache :: (Memcache.Client -> PluginM a) -> (a -> next) -> PluginF next
deriving instance Functor PluginF

newtype PluginM a = PluginM { unPluginM :: F PluginF a }
  deriving (Applicative, Functor)

instance Monad PluginM where
  return = PluginM . return
  PluginM a >>= b = PluginM (a >>= unPluginM . b)
  PluginM a >> PluginM b = PluginM (a >> b)
  fail err = PluginM (liftF (Fail err))

instance MonadThrow PluginM where
  throwM e = fail (show e)

instance Alternative PluginM where
  empty = fail "mzero{PluginM}"
  a <|> _ = a

instance MonadIO PluginM where
  liftIO fn = PluginM (liftF (DoIO fn id))

getPluginName :: PluginM ApiName
getPluginName = PluginM (liftF (GetPluginName id))

getPluginConfig :: PluginM HapiValue
getPluginConfig = PluginM (liftF (GetPluginConfig id))

withMemcacheClient :: (Memcache.Client -> PluginM a) -> PluginM a
withMemcacheClient go =
  PluginM (liftF (WithMemcache go id))

mkRequest :: ApiName -> ApiRequestName -> TypeName -> HapiObject
          -> PluginM (PluginType, HapiValue)
mkRequest api req argTy arg =
  PluginM (liftF (MkRequest api req (Just (argTy, arg)) (,)))

getVariable :: ApiName -> ApiRequestName -> PluginM (PluginType, HapiValue)
getVariable api req =
  PluginM (liftF (MkRequest api req Nothing (,)))

getDefaultRef :: TypeName -> PluginM HapiValue
getDefaultRef tyName = PluginM (liftF (GetDefault tyName id))

getCurrentTimePlugin :: PluginM UTCTime
getCurrentTimePlugin = PluginM (liftF (GetCurrentTime id))

getControlForType :: TypeName -> HapiObject -> PluginM ApiSettingsControl
getControlForType tyNm obj =
  PluginM (liftF (GetControlForType tyNm obj id))

normalizeExternalType :: TypeName -> HapiObject -> PluginM HapiObject
normalizeExternalType tyName obj = PluginM (liftF (Normalize tyName obj id))

getFileNameFromURI :: Text -> PluginM (Maybe Text)
getFileNameFromURI uri =
  case uri ^? unpacked . to parseURI . _Just . to uriAuthority
            . _Just . to uriRegName . packed . re _FileId . fileIdUuid of
    Nothing -> pure Nothing
    Just fileUuid -> PluginM (liftF (GetFileName fileUuid id))

pluginReadFile :: FileId -> PluginM B.ByteString
pluginReadFile fileId =
  case fileId ^? fileIdUuid of
    Nothing -> fail "Invalid file ID"
    Just fileId' -> PluginM (liftF (ReadFile fileId' id))

asynchronous :: forall a. FromHapiObject a => Int -> (AsyncToken -> String) -> PluginM a
asynchronous timeout mkUrl = do
  obj <- PluginM (liftF (GoAsync timeout (fromHapiObjectTypeName (Proxy :: Proxy a)) mkUrl id))
  case runHapiParser $ decodeHapiObject obj of
    Left err -> fail ("Could not decode object: " ++ err)
    Right x -> pure x

pluginLookup :: (ToHapiValue k, FromHapiObject v) => LookupName k v -> k ->  PluginM (Maybe v)
pluginLookup (LookupName lookupNm) key = do
  case encodeHapiValue key of
    HapiValueObject _ keyObj -> do
      valObj <- PluginM (liftF (GetInLookup lookupNm keyObj id))
      case valObj of
        Nothing -> pure Nothing
        Just valObj' ->
          case runHapiParser $ decodeHapiObject valObj' of
            Left err -> fail ("Could not decode object: " ++ err)
            Right x -> pure (Just x)
    _ -> pure Nothing

pluginLookupSet :: (ToHapiValue k, ToHapiValue v) => LookupName k v -> k -> v -> PluginM ()
pluginLookupSet (LookupName lookupNm) key val =
  case (encodeHapiValue key, encodeHapiValue val) of
    (HapiValueObject _ keyObj, HapiValueObject _ valObj) -> do
      PluginM (liftF (SetLookup lookupNm keyObj valObj ()))
    _ -> pure ()

-- | Saves the given value as part of the async and then returns a url that -- when accessed -- will restart the flow
kickAsync :: ToHapiValue a => AsyncToken -> a -> PluginM Text
kickAsync tok a =
  case encodeHapiValue a of
    HapiValueObject aTy aObj ->
      PluginM (liftF (KickAsync tok aTy aObj id))
    _ -> fail "kickAsync: need object"

putJSONLn :: ToJSON a => a -> IO ()
putJSONLn = BS.putStrLn . encode

readJSONLn :: FromJSON a => IO a
readJSONLn = do
  ln <- B.getLine
  Just x <- pure $ decodeStrict ln
  pure x

parseInPlugin :: HapiParser a -> PluginM a
parseInPlugin p =
  case runHapiParser p of
    Left e -> fail e
    Right a -> pure a

newtype HapiParser a =
  HapiParser { runHapiParser :: Either String a }
  deriving (Functor, Applicative, Alternative)
instance Monad HapiParser where
  fail = HapiParser . Left
  HapiParser a >>= mkB =
    HapiParser (a >>= runHapiParser . mkB)
  return = pure

class FromHapiObject a where
  fromHapiObjectTypeName :: Proxy a -> TypeName
  decodeHapiObject :: HapiObject -> HapiParser a
class ToHapiValue a where
  encodeHapiValue :: a -> HapiValue
instance ToHapiValue a => ToHapiValue [a] where
  encodeHapiValue = HapiValueList . map encodeHapiValue
instance ToHapiValue a => ToHapiValue (Maybe a) where
  encodeHapiValue Nothing = HapiValueNothing
  encodeHapiValue (Just x) = HapiValueJust $ encodeHapiValue x
instance ToHapiValue Integer where
  encodeHapiValue = HapiValueNumber . fromIntegral
instance ToHapiValue Int where
  encodeHapiValue = HapiValueNumber . fromIntegral
instance ToHapiValue Scientific where
  encodeHapiValue = HapiValueNumber
instance ToHapiValue Double where
  encodeHapiValue = HapiValueNumber . fromRational . toRational
instance ToHapiValue Text where
  encodeHapiValue = HapiValueText
instance ToHapiValue Bool where
  encodeHapiValue = HapiValueSwitch
instance ToHapiValue FileUrl where
  encodeHapiValue (FileUrl f) = HapiValueFile f
instance ToHapiValue UTCTime where
  encodeHapiValue = HapiValueTimestamp
instance (ToHapiValue a, ToHapiValue b) => ToHapiValue (a, b) where
  encodeHapiValue (a, b) = HapiValueTuple [encodeHapiValue a, encodeHapiValue b]

instance ToHapiValue HapiHttpRequest where
  encodeHapiValue (HapiHttpRequest method path' isSecure params body headers) =
    HapiValueObject (TypeName (Just "com.hapi") "Request") . HapiObject $
    HM.fromList [ ("method", encodeHapiValue method)
                , ("path", encodeHapiValue path')
                , ("isSecure", encodeHapiValue isSecure)
                , ("params", encodeHapiValue params)
                , ("body", encodeHapiValue body)
                , ("headers", encodeHapiValue headers)
                ]
instance FromHapiObject HapiHttpRequest where
  fromHapiObjectTypeName _ = TypeName (Just "com.hapi") "Request"
  decodeHapiObject (HapiObject obj) =
    HapiHttpRequest <$> do { Just x <- pure (obj ^? at "method" . _Just . _HapiValueText)
                           ; pure x }
                    <*> pure (obj ^.. at "path" . _Just . _HapiValueList . each . _HapiValueText)
                    <*> do { Just x <- pure (obj ^? at "isSecure" . _Just . _HapiValueSwitch)
                           ; pure x }
                    <*> do { Just xs <- pure (obj ^? at "params" . _Just . _HapiValueList)
                           ; mapM (\x -> do { HapiValueTuple [HapiValueText nm, val] <- pure x
                                            ; case val of
                                                 HapiValueNothing -> pure (nm, Nothing)
                                                 HapiValueJust (HapiValueText t) -> pure (nm, Just t)
                                                 _ -> fail "Expected nothing or just in params" }) xs }
                    <*> do { Just x <- pure (obj ^? at "body" . _Just . _HapiValueText)
                           ; pure x }
                    <*> do { Just xs <- pure (obj ^? at "headers" . _Just . _HapiValueList)
                           ; mapM (\x -> do { HapiValueTuple [HapiValueText nm, HapiValueText val] <- pure x; pure (nm, val)}) xs }

instance FromHapiObject HapiHttpResponse where
  fromHapiObjectTypeName _ = TypeName (Just "com.hapi") "Response"
  decodeHapiObject (HapiObject obj) =
    HapiHttpResponse <$> do { Just x <- pure (obj ^? at "status" . _Just . _HapiValueNumber . to floor)
                            ; pure x }
                     <*> do { Just x <- pure (obj ^? at "phrase" . _Just . _HapiValueText)
                            ; pure x }
                     <*> do { Just x <- pure (obj ^? at "body" . _Just . _HapiValueText)
                            ; pure x }
                     <*> do { Just xs <- pure (obj ^? at "headers" . _Just . _HapiValueList)
                            ; mapM (\x -> do { HapiValueTuple [HapiValueText nm, HapiValueText val] <- pure x; pure (nm, val)}) xs }
instance ToHapiValue HapiHttpResponse where
  encodeHapiValue (HapiHttpResponse sts phrase body headers) =
    HapiValueObject (TypeName (Just "com.hapi") "Response") . HapiObject . HM.fromList $
    [ ("status", encodeHapiValue sts), ("phrase", encodeHapiValue phrase), ("body", encodeHapiValue body)
    , ("headers", encodeHapiValue headers) ]

-- runPluginSimple :: PluginM a -> IO a
-- runPluginSimple (PluginM plugin) =
--   runF plugin pure runStep
--   where
--     runStep (MkRequest api req (Just (typeNm, obj)) next) =
--       do putStrLn "MKREQUEST"
--          T.putStrLn (api ^. _ApiName)
--          T.putStrLn (req ^. _ApiRequestName)
--          putJSONLn typeNm
--          putJSONLn obj

--          join (next <$> readJSONLn <*> readJSONLn)

--     runStep (MkRequest api req Nothing next) =
--       do putStrLn "GETVAR"
--          T.putStrLn (api ^. _ApiName)
--          T.putStrLn (req ^. _ApiRequestName)

--          join (next <$> readJSONLn <*> readJSONLn)

--     runStep (GetDefault tyName next) =
--       do putStrLn "GETDEFAULT"
--          putJSONLn tyName

--          next =<< readJSONLn

--     runStep (Normalize tyName obj next) =
--       do putStrLn "NORMALIZE"
--          putJSONLn tyName
--          putJSONLn obj

--          next =<< readJSONLn

--     runStep (GetCurrentTime next) = next =<< getCurrentTime
