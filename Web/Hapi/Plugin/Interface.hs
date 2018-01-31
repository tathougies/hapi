module Web.Hapi.Plugin.Interface
 ( Plugin(..)
 , pluginMain
 , mkHapiConfigDefault
 , mkHapiValueLabel
 , runIdlExpression
 , varCtxt
 , varCtxtFromObj
 , doNormalize

 , settingsChangeAllowed
 , rerefFiles

 , cache, cacheJSON, cacheKeyJSON

 -- * HTTP client
 , FromHttpResponse(..)
 , HttpResponseJSON(..), HttpResponseFormData(..)
 , httpReq
 ) where

import           Web.Hapi.Plugin.Idl
import           Web.Hapi.Plugin.Manager
import           Web.Hapi.Plugin.Monad
import           Web.Hapi.Schema
import           Web.Hapi.Types

import           Control.Lens
import           Control.Monad

import           Data.Aeson hiding (Value)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as Strict
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Lens

import           Database.Beam hiding (lookup)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.Memcache.Client as Memcache

import           Network.URI
import           Network.URI.Encode (decodeByteString)
import           Network.HTTP.Types (parseQuery)
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP

import           Options.Generic

import qualified Text.Pandoc as Pandoc

import           Web.FormUrlEncoded

data PluginCmdLine
  = PluginName
  | PluginHandleRequest
  | PluginIdl
  | PluginDescribe

  | PluginServe

  deriving Generic

instance ParseRecord PluginCmdLine

newtype PluginContext
  = PluginContext (HM.HashMap Identifier HapiValue)
  deriving Monoid

pluginMain :: Plugin -> IO ()
pluginMain plugin = do
  cmd <- getRecord ("Hapi Plugin interface: " <> plugin ^. pluginName)

  case cmd of
    PluginName ->
      putStrLn (plugin ^. pluginName . unpacked)
    PluginHandleRequest -> pure ()
    PluginIdl -> putStrLn (plugin ^. pluginIdl . unpacked)
    PluginDescribe -> B.putStrLn (encode (plugin ^. pluginDescription))

    PluginServe ->
      clientPluginMain plugin

varCtxtFromObj :: HM.HashMap PropertyName HapiValue -> PluginContext
varCtxtFromObj dat = PluginContext (HM.fromList . map (\(nm, val) -> (nm ^. _PropertyName . re _Identifier, val)) . HM.toList $ dat)

mkHapiValueLabel :: HapiValue -> Text
mkHapiValueLabel (HapiValueText t) = t
mkHapiValueLabel (HapiValueHtml h) = h
mkHapiValueLabel (HapiValueNumber n) = fromString (show n)
mkHapiValueLabel (HapiValueTimestamp tm) = fromString (show tm)
mkHapiValueLabel (HapiValueJust x) = mkHapiValueLabel x
mkHapiValueLabel HapiValueNothing = mempty
mkHapiValueLabel (HapiValueSwitch _) = "<bool>"
mkHapiValueLabel (HapiValueCredential _) = "<secret>"
mkHapiValueLabel (HapiValueObject tyNm _) = fromString ("<object:" ++ show tyNm ++ ">")
mkHapiValueLabel (HapiValueFile _) = "<file>"
mkHapiValueLabel (HapiValueList _) = "<list>"
mkHapiValueLabel (HapiValueTuple _) = "<tuple>"

mkHapiConfigDefault :: PluginType -> PluginM HapiValue
mkHapiConfigDefault (PluginTypeMany _) = pure (HapiValueList [])
mkHapiConfigDefault (PluginTypeOptional _) = pure HapiValueNothing
mkHapiConfigDefault (PluginTypeTuple xs) = HapiValueTuple <$> mapM mkHapiConfigDefault xs
mkHapiConfigDefault (PluginTypeReference ref) = getDefaultRef ref
mkHapiConfigDefault (PluginTypeBuiltin BuiltinTypeString)    = pure (HapiValueText "")
mkHapiConfigDefault (PluginTypeBuiltin BuiltinTypeTimestamp) = HapiValueTimestamp <$> getCurrentTimePlugin
mkHapiConfigDefault (PluginTypeBuiltin BuiltinTypeBool)      = pure (HapiValueSwitch False)
mkHapiConfigDefault (PluginTypeBuiltin BuiltinTypeDecimal)   = pure (HapiValueNumber 0)
mkHapiConfigDefault (PluginTypeBuiltin BuiltinTypeNumber)    = pure (HapiValueNumber 0)
mkHapiConfigDefault (PluginTypeBuiltin BuiltinTypeFileUpload) = pure (HapiValueFile "")

runIdlExpression :: PluginContext -> Expression -> PluginM HapiValue
runIdlExpression _ (ExpressionApply (QualifiedIdentifier Nothing _) _) = fail "Won't resolve internal requests"
runIdlExpression ctxt (ExpressionApply (QualifiedIdentifier (Just pkg) req) args) = do
  argObject <- fmap HM.fromList . forM args $ \(lbl, e) ->
               (lbl ^. _Identifier . re _PropertyName,) <$> runIdlExpression ctxt e
  snd <$> mkRequest (pkg ^. _PackageName . re _ApiName) (req ^. _Identifier . re _ApiRequestName)
                    (TypeName (Just pkg) req) (HapiObject argObject)
runIdlExpression _ (ExpressionValue v) =
  pure (hapiValue v)
runIdlExpression (PluginContext ctxt) (ExpressionIdentifier (QualifiedIdentifier Nothing var))
  | Just v <- ctxt ^. at var = pure v
  | otherwise =
    fail "No variable resolution"
runIdlExpression _ (ExpressionIdentifier (QualifiedIdentifier (Just pkg) req)) =
  snd <$> getVariable (pkg ^. _PackageName . re _ApiName) (req ^. _Identifier . re _ApiRequestName)
runIdlExpression c (ExpressionString pieces) =
  fmap (HapiValueText . mconcat) .
  forM pieces $ \pc ->
    case pc of
      StringPieceText t -> pure t
      StringPieceExpression e ->
        mkHapiValueLabel <$> runIdlExpression c e
runIdlExpression c (ExpressionMarkdown pieces) =
  do HapiValueText t <- runIdlExpression c (ExpressionString pieces)
     case Pandoc.readMarkdown Pandoc.def (T.unpack t) of
       Left err -> fail ("Could not process markdown " ++ show err)
       Right doc -> pure (HapiValueHtml (fromString (Pandoc.writeHtmlString Pandoc.def doc)))
runIdlExpression ctxt (ExpressionGetProperty e (Identifier nm)) =
  do v <- runIdlExpression ctxt e
     case v of
       HapiValueObject _ (HapiObject v')
         | Just v'' <- v' ^. at (PropertyName nm) -> pure v''
       _ -> fail "No such property on non-object"
runIdlExpression _ _ = fail "No more idl exprs"

hapiValue :: Value -> HapiValue
hapiValue (ValueNumber i) = HapiValueNumber (fromIntegral i)
hapiValue (ValueString t) = HapiValueText t
hapiValue (ValueTimestamp ts) = HapiValueTimestamp ts
hapiValue (ValueBool b) = HapiValueSwitch b
hapiValue (ValueDecimal d) = HapiValueNumber d

varCtxt :: Identifier -> HapiValue -> PluginContext
varCtxt nm v = PluginContext (HM.singleton nm v)

doNormalize :: PluginCompoundType -> HapiObject -> PluginM HapiObject
doNormalize ty (HapiObject obj) =
  HapiObject <$> foldlMOf compoundTypeAllFields addField mempty ty
  where
    addField obj' field
      | includeField field = do
          let fieldNm = field ^. compoundFieldName . _Identifier . re _PropertyName
          fieldValue <- maybe (mkHapiConfigDefault (field ^. compoundFieldType)) pure (obj ^. at fieldNm)
          fieldValue' <-
            case (fieldValue, field ^. compoundFieldType) of
              (HapiValueObject ty' obj'', PluginTypeReference tyNm)
                | ty' == tyNm ->
                  HapiValueObject ty' <$> normalizeExternalType tyNm obj''
              _ -> pure fieldValue

          pure (obj' & at fieldNm ?~ fieldValue')
      | otherwise = pure obj'

    includeField field
      | Just AttributeValueOff <- findAttribute "store" (field ^. compoundFieldAttrs) = False
      | otherwise = True

settingsChangeAllowed :: Pg.Connection -> HapiObject -> HapiObject -> IO (Maybe HapiObject)
settingsChangeAllowed conn oldSettings newSettings = do
  let oldFileIds = HS.fromList $ oldSettings ^.. hapiObjectValues . hapiValueFiles . unpacked
                                   . to parseURI . _Just . to uriAuthority . _Just
                                   . to uriRegName . packed . re _FileId . fileIdUuid

      idAndSecret uri =
        case uriAuthority uri of
          Nothing -> Nothing
          Just auth ->
            let queryString = uriQuery uri
                queryData = parseQuery (fromString queryString)
                queryText = TE.decodeUtf8 . decodeByteString <$> join (lookup "secret" queryData)
            in (, queryText) <$> (uriRegName auth ^? packed . re _FileId . fileIdUuid)
      newParsedFileIds = newSettings ^.. hapiObjectValues . hapiValueFiles . unpacked
                           . to parseURI . _Just . to idAndSecret . _Just

      unauthedFileIds = (HS.fromList $ map fst $ filter (isNothing . snd) newParsedFileIds) `HS.difference` oldFileIds

  if not (HS.null unauthedFileIds)
    then pure Nothing
    else
      -- Check that the ones that were authed are true
      let authedFileIds = newParsedFileIds ^.. each . to sequence . _Just

          cleanedUpSettings = newSettings & hapiObjectValues . hapiValueFiles . unpacked %~ rewriteFileUri
          rewriteFileUri fileStr =
            case parseURI fileStr of
              Nothing -> fileStr
              Just fileUri ->
                show (fileUri { uriQuery = "" })

          doAuth [] = pure (Just cleanedUpSettings)
          doAuth ((fileId, secret):xs) = do
            res <- withDatabase conn $
                   runSelectReturningOne $ select $ pure $
                   exists_ $ do
                     upload <- all_ (hapiDb ^. dbUploads)
                     guard_ (upload ^. fileUploadId ==. val_ fileId &&.
                             upload ^. fileUploadSecret ==. val_ secret)
                     pure (upload ^. fileUploadId)
            case res of
              Just True -> doAuth xs
              _ -> pure Nothing
      in doAuth authedFileIds

rerefFiles :: Pg.Connection -> HapiObject -> HapiObject -> IO ()
rerefFiles conn oldSettings newSettings = do
  let allFileIds = hapiObjectValues . hapiValueFiles . unpacked
                 . to parseURI . _Just . to uriAuthority . _Just
                 . to uriRegName . packed . re _FileId . fileIdUuid

      oldFileIds = HS.fromList $ oldSettings ^.. allFileIds
      newFileIds = HS.fromList $ newSettings ^.. allFileIds

      dereffedFileIds = HS.toList (oldFileIds `HS.difference` newFileIds)
      reffedFileIds = HS.toList (newFileIds `HS.difference` oldFileIds)

  withDatabaseDebug putStrLn conn $ do
    if null dereffedFileIds then pure ()
      else runUpdate $
           update (hapiDb ^. dbUploads)
                  (\upload -> [(upload ^. fileUploadUsed) <-.
                                current_ (upload ^. fileUploadUsed) - 1] )
                  (\upload -> (upload ^. fileUploadId) `in_` map val_ dereffedFileIds)

    if null reffedFileIds then pure ()
      else runUpdate $
           update (hapiDb ^. dbUploads)
                  (\upload -> [(upload ^. fileUploadUsed) <-.
                                current_ (upload ^. fileUploadUsed) + 1] )
                  (\upload -> (upload ^. fileUploadId) `in_` map val_ reffedFileIds)

cacheJSON :: (ToJSON k, ToJSON a, FromJSON a)
      => Maybe Int
      -> k -> PluginM a
      -> PluginM a
cacheJSON timeout key =
  cacheKeyJSON timeout key (B.toStrict . encode) decodeStrict

cacheKeyJSON :: ToJSON k
             => Maybe Int -> k
             -> (a -> Strict.ByteString) -> (Strict.ByteString -> Maybe a)
             -> PluginM a
             -> PluginM a
cacheKeyJSON timeout' key encodeValue decodeValue mkValue = do
  ApiName pluginName' <- getPluginName
  let memcacheKey = TE.encodeUtf8 pluginName' <> ":" <> B.toStrict (encode key)
  cache timeout' memcacheKey encodeValue decodeValue mkValue

cache :: Maybe Int -> Strict.ByteString -> (a -> Strict.ByteString)
      -> (Strict.ByteString -> Maybe a) -> PluginM a -> PluginM a
cache timeout' key encodeValue decodeValue mkValue =
  withMemcacheClient $ \client ->
  do let timeout = fromMaybe 0 timeout'
     val <- liftIO $ Memcache.get client key
     case val of
       Just (valBs, _, _)
         | Just v <- decodeValue valBs ->
             pure v
       _ -> do
         v <- mkValue
         let v' = encodeValue v
         _ <- liftIO $ Memcache.set client key v' 0 (fromIntegral timeout)
         pure v

-- HTTP client utils

newtype HttpResponseJSON a = HttpResponseJSON { fromHttpResponseJSON :: a }
  deriving Show
newtype HttpResponseFormData a = HttpResponseFormData { fromHttpResponseFormData :: a }
  deriving Show
class FromHttpResponse d where
  fromHttpResponse :: HTTP.Response B.ByteString -> Maybe d
instance FromJSON d => FromHttpResponse (HttpResponseJSON d) where
  fromHttpResponse = fmap HttpResponseJSON . decode . HTTP.responseBody
instance FromForm d => FromHttpResponse (HttpResponseFormData d) where
  fromHttpResponse d =
    either (\_ -> Nothing) (Just . HttpResponseFormData)
           (urlDecodeForm (HTTP.responseBody d) >>= fromForm)

httpReq :: (MonadIO m, FromHttpResponse d)
        => HTTP.Request -> (Int -> Bool)
        -> m (Either (HTTP.Response B.ByteString) d)
httpReq req statusOk = do
  resp <- liftIO $ do
    mgr <- HTTP.getGlobalManager
    HTTP.httpLbs req mgr
  case HTTP.responseStatus resp of
    HTTP.Status sts _
      | statusOk sts ->
          case fromHttpResponse resp of
            Nothing -> pure (Left resp)
            Just  x -> pure (Right x)
    _ -> pure (Left resp)
