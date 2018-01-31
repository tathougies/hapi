module Main where

import           Prelude hiding (lookup)

import           HapiAPI

import           Web.Hapi.Config
import           Web.Hapi.File
import           Web.Hapi.File.Local
import           Web.Hapi.Plugin.Idl
import           Web.Hapi.Plugin.Interface
import           Web.Hapi.Plugin.Manager
import           Web.Hapi.Plugin.Monad
import           Web.Hapi.Registry
import qualified Web.Hapi.Schema as Db
import           Web.Hapi.Schema hiding (User, UserT(..), userId, userHomepage)
import           Web.Hapi.Types

import           Control.Concurrent.STM
import           Control.Exception (catch)
import           Control.Lens
import           Control.Monad.Except

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as StrictBS
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.CaseInsensitive (CI)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Pool as P
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (getCurrentTime, addUTCTime)
import           Data.Time.Clock.POSIX
import           Data.UUID (UUID)

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Migrate.Simple
import qualified Database.Beam.Postgres as Pg
import qualified Database.PostgreSQL.Simple as Db
import qualified Database.Memcache.Client as Memcache

import           Network.BSD
import           Network.HTTP.Types
import qualified Network.URI.Encode as URI
import           Network.Wai
import           Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import           Network.Wai.Handler.Warp hiding (FileInfo)
import           Network.Wai.Parse

import           Options.Generic

import           System.IO

import           Servant

data HapiOptions
  = HapiOptions
  { port   :: Int <?> "Port to run on"
  , config :: FilePath <?> "Path to config file"
  , migrate :: Bool <?> "Attempt database migration"
  } deriving Generic

instance ParseRecord HapiOptions

setupAsync :: PluginManager -> Int -> AsyncToken -> TypeName -> String
           -> NextAsync (PluginType, HapiValue)
           -> IO [(CI StrictBS.ByteString, StrictBS.ByteString)]
setupAsync mgr timeout (AsyncToken token) tyName url doNext = do
  tokenSecret <- randomString

  -- Save the async token along with the host to redirect to
  hPutStrLn stderr ("Save async token " ++ show token)
  _ <- P.withResource (mgr ^. pluginManagerMemcachePool) $ \client -> do
    hostname <- getHostName
    let key = "com.hapi.async:" <> TE.encodeUtf8 token
        value = BS.toStrict (JSON.encode (HapiAsyncInfo (fromString hostname) tyName tokenSecret))
    Memcache.set client key value 0 (fromIntegral timeout)

  curTime <- getPOSIXTime
  atomically $ do
    asyncs <- readTVar (mgr ^. pluginManagerAsyncs)
    case asyncs ^. at (AsyncToken token) of
      Nothing-> do
        condVar <- newEmptyTMVar
        reqVar  <- newEmptyTMVar
        respVar <- newTMVar doNext
        modifyTVar' (mgr ^. pluginManagerAsyncs) $
          at (AsyncToken token) ?~ (curTime + fromIntegral timeout, condVar, reqVar, respVar)
      Just (_, _, _, nextVar) -> do
        _ <- tryTakeTMVar nextVar
        putTMVar nextVar doNext

  curUTCTime <- getCurrentTime
  let expTime = fromIntegral timeout `addUTCTime` curUTCTime

  hPutStrLn stderr ("TODO: Save token " ++ show token ++ " " ++ show tokenSecret)

  pure [ ("Location", fromString url)
       , ("Expires", formatHttpTime expTime)
       , ("X-Hapi-Async-Token", TE.encodeUtf8 token)
       , ("X-Hapi-Async-Secret", TE.encodeUtf8 tokenSecret)
       ]

apiEndpoint :: P.Pool Db.Connection -> PluginManager
            -> UserId -> ApiId -> ApiRequestName
            -> Server RequestAPI
apiEndpoint pool mgr userId' apiId' reqNm
  | Just userUuid <- userId' ^? userIdUuid
  , Just apiUuid  <- apiId' ^? apiIdUuid =
      let verifyRequest = do
            api <-
              Handler $ ExceptT $ P.withResource pool $ flip withDatabase $ runExceptT $ do
                user <- lift $ runSelectReturningOne $ lookup (hapiDb ^. Db.dbUsers) (UserKey userUuid)
                case user of
                  Nothing -> throwError err404
                  Just {} -> do
                    api <- lift $ runSelectReturningOne $ select $
                           filter_ (\api -> api ^. apiUser ==. val_ (UserKey userUuid) &&.
                                            api ^. Db.apiId ==. val_ apiUuid) $
                           all_ (hapiDb ^. Db.dbApis)
                    maybe (throwError err404) pure api

            let apiNm = ApiName (api ^. Db.apiName)
            st <- Handler $ ExceptT $
                  fmap Right (getPluginSt mgr apiNm) `catch`
                  (\(NoSuchPlugin _) -> pure (Left $ err500 { errBody = "Could not find plugin"}))

            let req = st ^? pluginStateApiDescription . apiDescriptionRequests . each .
                            filtered (\req' -> req' ^. apiRequestName == reqNm &&
                                               (req' ^. apiRequestVisible == ApiVisibilityPrivate ||
                                                req' ^. apiRequestVisible == ApiVisibilityPublic))
            case req of
              Nothing -> throwError err404
              Just req' -> pure (req', api, apiNm)

          performRequest args = do
            (_, _, apiNm) <- verifyRequest
            res <-
              Handler $ withResponseError $ do
                let scp = ScopeContextApi userId' apiId'
                args' <- ExceptT $ requestFromPlugin mgr apiNm (PluginRequestFill scp (reqNm ^. _ApiRequestName . re _Identifier) args)
                ExceptT $ requestFromPluginAsync mgr apiNm (PluginRequestPerform scp reqNm args')
            case res of
              AsyncResponseContinues timeout token tyName url doNext -> do
                hdrs <- liftIO $ setupAsync mgr timeout token tyName url doNext
                throwError err303 { errReasonPhrase = "Asynchronous"
                                  , errBody = "{}"
                                  , errHeaders = hdrs }
              AsyncResponseDone (_, obj) ->
                pure obj

          getRequest = do
            (_, _, apiNm) <- verifyRequest
            Handler $ withResponseError $ do
              let scp = ScopeContextApi userId' apiId'
                  reqIdentifier = reqNm ^. _ApiRequestName . re _Identifier
              args <- ExceptT (requestFromPlugin mgr apiNm (PluginRequestFill scp reqIdentifier (HapiObject mempty)))
              res <- ExceptT (requestFromPlugin mgr apiNm (PluginRequestForm scp reqIdentifier args))
              case res of
                Left  _ -> throwError (PluginError "com.hapi" "Got control instead of form")
                Right f -> pure (ApiRequestInstanceDescription f)

      in getRequest :<|> performRequest
  | otherwise = throwAll err404

userServer :: P.Pool Db.Connection -> UserId -> Server UserAPI
userServer dbPool uid = getUser :<|> putUser :<|> deleteUser
  where
    getUser
      | Just userUuid <- uid ^? userIdUuid = do
          liftIO $ putStrLn ("GOt user id " ++ show userUuid)
          dbUser <-
            liftIO $ P.withResource dbPool $
            flip withDatabase $
            runSelectReturningOne $ lookup (hapiDb ^. dbUsers) (UserKey userUuid)
          case dbUser of
            Nothing -> throwError err404
            Just dbUser' -> pure (User (review userIdUuid (dbUser' ^. Db.userId)) (dbUser' ^. Db.userHomepage))
      | otherwise = liftIO (putStrLn "No parse") >> throwError err404

    putUser user
      | user ^. userId /= uid = throwError err400
      | Just userUuid <- uid ^? userIdUuid = do
          liftIO $ P.withResource dbPool $ flip withDatabase $
            runUpdate $
            update (hapiDb ^. dbUsers)
                   (\userT -> [ (userT ^. Db.userHomepage) <-. val_ (user ^. userHomepage) ])
                   (\userT -> (userT ^. Db.userId) ==. val_ userUuid)
          pure user
      | otherwise = throwError err400

    deleteUser
      | Just userUuid <- uid ^? userIdUuid =
          liftIO $ P.withResource dbPool $ flip withDatabase $
          runDelete $ delete (hapiDb ^. dbUsers) (\u -> u ^. Db.userId ==. val_ userUuid)
      | otherwise = pure ()

assertUserExists :: (MonadError ServantErr m, MonadIO m) => P.Pool Db.Connection -> UUID -> m ()
assertUserExists dbPool uid = do
  dbUser <-
    liftIO $ P.withResource dbPool $
    flip withDatabase $
    runSelectReturningOne $ lookup (hapiDb ^. dbUsers) (UserKey uid)
  case dbUser of
    Nothing -> throwError err404
    Just {} -> pure ()

withResponseError :: Functor m => ExceptT PluginError m a -> ExceptT ServantErr m a
withResponseError = withExceptT (\e -> case e of
                                         PluginAsync _ (AsyncToken t) _ redirect ->
                                           err303 { errReasonPhrase = "Asynchronous"
                                                  , errBody = "Plugin went into async mode unexpectedly"
                                                  , errHeaders = [ ("Location", fromString redirect)
                                                                 , ("X-Hapi-Async-Token", TE.encodeUtf8 t) ] }
                                         PluginError _ err -> err500 { errBody = fromString (T.unpack err) })

userApiServer :: P.Pool Db.Connection -> PluginManager -> UserKey -> ApiId -> Server ApiInstanceAPI
userApiServer dbPool mgr userKey@(UserKey userUuid) apiId_
  | Just apiUuid <- apiId_ ^? apiIdUuid =
      let uid = review userIdUuid userUuid

          lookupApiInstance =
            flip withDatabase $
            runSelectReturningOne $ select $
            filter_ (\api -> api ^. apiUser ==. val_ userKey &&.
                             api ^. apiId   ==. val_ apiUuid) $
            all_ (hapiDb ^. dbApis)

          getApiStatus apiNm apiConfig = do
            let scp = ScopeContextApi (userUuid ^. re userIdUuid) apiId_
            configTyNm <- ExceptT $ requestFromPlugin mgr (ApiName apiNm) PluginRequestConfigTypeName
            Identifier reqNm <- ExceptT $ requestFromPlugin mgr (ApiName apiNm) PluginRequestConfigRequest
            (retType, ret) <-
              ExceptT $ requestFromPlugin mgr (ApiName apiNm)
                          (PluginRequestPerform scp (ApiRequestName reqNm)
                                                (HapiObject (HM.singleton "new"
                                                              (HapiValueObject (TypeName (Just (PackageName apiNm)) configTyNm)
                                                                               apiConfig))))
            case (retType, ret) of
              (PluginTypeMany (PluginTypeBuiltin BuiltinTypeString), HapiValueList errors)
                | null errors -> pure ApiStatusUp
                | otherwise -> pure (ApiStatusError (errors ^.. each . _HapiValueText))
              _ -> throwError (PluginError "com.hapi" "Type mismatch when fetching status")

          getApiInstance = do
            api <- liftIO $ P.withResource dbPool lookupApiInstance
            case api of
              Nothing -> throwError err404
              Just (ApiRegistration { _apiName = nm, _apiData = Pg.PgJSONB settings }) -> do
                let scp = ScopeContextApi (userUuid ^. re userIdUuid) apiId_
                settings' <- Handler . withResponseError $ do
                  configTyNm <- ExceptT $ requestFromPlugin mgr (ApiName nm) PluginRequestConfigTypeName
                  ExceptT $ requestFromPlugin mgr (ApiName nm) (PluginRequestFill scp configTyNm settings)
                sts <- Handler . withResponseError $ getApiStatus nm settings'
                case mgr ^. pluginManagerRegistry . registryApis . at (ApiName nm) of
                  Nothing -> throwError err404
                  Just plugin ->
                    pure ActivatedApi { _activatedApiId = apiId_
                                      , _activatedApiApi = ApiName nm
                                      , _activatedApiApiUrl = Just (linkURI (safeLink hapiApi describeUserApiLink uid (ApiName nm)))
                                      , _activatedApiStatus = sts
                                      , _activatedApiSettings = settings'
                                      , _activatedApiSettingsUrl = Just (linkURI (safeLink hapiApi getApiSettingsLink uid apiId_))
                                      , _activatedApiDescription = plugin ^. to (crossLinkPlugin (apiLinkifier (userUuid ^. re userIdUuid) apiId_)) . pluginRegistrationDescription}

          getApiInstanceSettings = do
            ActivatedApi { _activatedApiSettings = settings
                         , _activatedApiApi = apiNm
                         , _activatedApiStatus = status } <- getApiInstance
            Handler $ withResponseError $ do
                let scp = ScopeContextApi (userUuid ^. re userIdUuid) apiId_
                configTyNm <- ExceptT $ requestFromPlugin mgr apiNm PluginRequestConfigTypeName
                liftIO $ hPutStrLn stderr ("Filled settings" ++ show settings)
                res <- ExceptT $ requestFromPlugin mgr apiNm (PluginRequestForm scp configTyNm settings)
                case res of
                  Left  _ -> throwError (PluginError "com.hapi" "Only got one control for form")
                  Right apiSettingsDescr ->
                    pure (apiSettingsDescr & apiSettingsErrors .~ (status ^. _ApiStatusError))

          putApiInstanceSettings settings =
            Handler . ExceptT . P.withResource dbPool $ \hdl -> runExceptT $ do
              api <-  liftIO $ lookupApiInstance hdl
              case api of
                Nothing -> throwError err404
                Just ApiRegistration { _apiData = Pg.PgJSONB oldSettings
                                     , _apiName = apiNm } -> do
                  -- Check access tokens
                  liftIO $ putStrLn ("Put settings " ++ show settings)

                  settings' <-
                    liftIO $ settingsChangeAllowed hdl oldSettings settings
                  case settings' of
                    Nothing -> throwError err400
                    Just settings'' ->
                      withResponseError $ do
                        liftIO $ rerefFiles hdl oldSettings settings

                        let apiNm' = ApiName apiNm
                            scp = ScopeContextApi (userUuid ^. re userIdUuid) apiId_

                        configTyNm  <- ExceptT $ requestFromPlugin mgr apiNm' PluginRequestConfigTypeName
                        settings''' <- ExceptT $ requestFromPlugin mgr apiNm' (PluginRequestFill scp configTyNm settings'')

                        liftIO $ withDatabase hdl $
                          runUpdate $ update (hapiDb ^. dbApis)
                                             (\api' -> [ (api' ^. apiData) <-. val_ (Pg.PgJSONB settings''') ])
                                             (\api' -> api' ^. apiId ==. val_ apiUuid)

                        status <- getApiStatus apiNm settings'''
                        res <- ExceptT $ requestFromPlugin mgr apiNm' (PluginRequestForm scp configTyNm settings''') 
                        case res of
                          Left {} -> throwError (PluginError "com.hapi" "Got control for form")
                          Right apiSettingsDescr ->
                            pure (apiSettingsDescr & apiSettingsErrors .~ (status ^. _ApiStatusError))

      in getApiInstance :<|>
         error "Can't describe api instance lookup" :<|>
         getApiInstanceSettings :<|> putApiInstanceSettings :<|>
         error "can't delete api"
  | otherwise = throwAll err404

apiServer :: P.Pool Db.Connection -> PluginManager -> UserId -> Server ApiAPI
apiServer dbPool mgr uid
  | Just userUuid <- uid ^? userIdUuid =
      let getApisStatus = do
            apis <-
              liftIO $ P.withResource dbPool $ flip withDatabase $
              runSelectReturningList $ select $ do
                api <- all_ (hapiDb ^. dbApis)
                guard_ (val_ (UserKey userUuid) ==. api ^. apiUser)
                pure api
            pure ApisStatus
              { _apisStatusActive =
                  flip map apis $ \api ->
                    let apiId' =  api ^. apiId . re apiIdUuid
                    in ActiveApiLink
                       { _activeApiLinkId = apiId'
                       , _activeApiLinkUrl = linkURI $ safeLink hapiApi getApiLink uid apiId'
                       }
              , _apisStatusAvailable =
                  flip map (mgr ^.. pluginManagerRegistry . registryApis . each) $ \api ->
                    AvailableApiLink
                    { _availableApiLinkName = api ^. pluginRegistrationName
                    , _availableApiLinkDescriptionUrl = linkURI $ safeLink hapiApi describeApiLink (api ^. pluginRegistrationName)
                    , _availableApiLinkCreateUrl = linkURI $ safeLink hapiApi describeUserApiLink uid (api ^. pluginRegistrationName)
                    }
              }

          userApiDescriptionApi nm
            | Just plugin <- mgr ^. pluginManagerRegistry . registryApis . at nm  =
                ( assertUserExists dbPool userUuid >>
                  pure (set apiDescriptionCreateUrl (Just . linkURI $ safeLink hapiApi createUserApiLink uid) .
                        view pluginRegistrationDescription $
                        crossLinkPlugin (userLinkifier uid) plugin) ) :<|>
                error "api server"
            | otherwise = throwAll err404

          createUserApi (ApiCreationPayload apiNm)
            | Just _ <- mgr ^. pluginManagerRegistry . registryApis . at apiNm = do
                newApiId <-
                  Handler $ ExceptT $ liftIO $ P.withResource dbPool $ \conn ->
                  runExceptT $ withResponseError $ do
                    [ apiReg ] <- liftIO $ withDatabase conn $
                      runInsertReturningList (hapiDb ^. dbApis) $
                      insertExpressions [ Db.ApiRegistration default_ (val_ (apiNm ^. _ApiName)) default_ (UserKey (val_ userUuid))
                                                             (val_ (Pg.PgJSONB (HapiObject mempty))) ]

                    let newApiId = apiReg ^. apiId . re apiIdUuid
                        scp = ScopeContextApi uid newApiId

                    (tyIdentifier, obj) <- ExceptT $ requestFromPlugin mgr apiNm (PluginRequestInitialConfig scp)
                    liftIO $ putStrLn ("Got " ++ show obj)
                    obj' <- ExceptT $ requestFromPlugin mgr apiNm (PluginRequestNormalize scp tyIdentifier obj)
                    liftIO $ putStrLn ("Got " ++ show obj' ++ " after normalization")

                    liftIO $ withDatabase conn $
                      runUpdate $
                      update (hapiDb ^. dbApis)
                             (\api -> [ (api ^. apiData) <-. val_ (Pg.PgJSONB obj') ])
                             (\api -> (api ^. apiId) ==. val_ (apiReg ^. apiId))

                    pure newApiId

                redirect303 (safeLink hapiApi getApiLink uid newApiId)
            | otherwise = throwAll err400
      in getApisStatus :<|> createUserApi :<|> userApiServer dbPool mgr (UserKey userUuid) :<|>
         userApiDescriptionApi
  | otherwise = throwAll err404

describeApiServer :: PluginManager -> ApiName -> Server ApiDescriptionAPI
describeApiServer mgr nm
  | Just plugin <- mgr ^. pluginManagerRegistry . registryApis . at nm =
      let describeLookup _ = throwError err404
      in pure (plugin ^. pluginRegistrationDescription) :<|>
         describeLookup
  | otherwise = throwAll err404

fileUploadEndpoint :: P.Pool Db.Connection -> HapiFileBackend -> Application
fileUploadEndpoint pool be req resp =
  do let opts = setMaxRequestFileSize (10 * 1024 * 1024) $
                setMaxRequestNumFiles 1 $
                defaultParseRequestBodyOptions

     (params, files) <- parseRequestBodyEx opts (fileBackendSave be) req
     case (params, files) of
       ([], _) -> resp (responseLBS status400 [] "Bad request")
       (_, []) -> resp (responseLBS status400 [] "No files given")
       (_, _:_:_) -> resp (responseLBS status400 [] "Too many files")
       ([("secret", secretTxt)], [("file", FileInfo fileName' fileContentType' (Just (fileId, fileData)))])
         | StrictBS.length secretTxt > 64 -> do
           let secretTxt' = TE.decodeUtf8 secretTxt
               fileNameTxt = TE.decodeUtf8 fileName'
           P.withResource pool $ flip withDatabase $
             runInsert $ insert (hapiDb ^. dbUploads) $
             insertExpressions [ Db.FileUpload (val_ fileId ) (val_ fileNameTxt)
                                               (val_ (TE.decodeUtf8 fileContentType'))
                                               (val_ (Pg.PgJSONB fileData))
                                               default_ (val_ 0) (val_ secretTxt') ]

           let fileUrl = "hapi-file://" <> fileId ^. re fileIdUuid . _FileId <> "?secret=" <>
                         URI.encodeText secretTxt'
           resp (responseLBS status201 [] (JSON.encode (ApiSettingsControlFileUpload (Just (fileNameTxt, fileUrl)))))
       _ -> resp (responseLBS status400 [] "Bad request")

publicEndpoint :: P.Pool Db.Connection -> PluginManager -> Text -> Tagged Handler Application
publicEndpoint pool mgr publicEndpointNm =
  Tagged $ \req resp ->
  case fromPublicEndpointLink publicEndpointNm of
    Nothing -> resp (responseLBS status404 [] "Not found")
    Just (Continuation asyncTok) -> doContinuation asyncTok req resp
    Just (Endpoint endpointNm) -> doEndpoint endpointNm req resp
  where
    doEndpoint endpointNm req resp = do
      liftIO $ hPutStrLn stderr $ "Public endpoint " ++ show endpointNm
      endpoint <-
        P.withResource pool $ flip withDatabase $
          runSelectReturningOne $ lookup (hapiDb ^. dbPublicEndpoints) (PublicEndpointKey endpointNm)
      case endpoint of
        Nothing -> resp (responseLBS status404 [] "Not found")
        Just endpoint' -> do
          let scope = case (endpoint' ^. endpointUser, endpoint' ^. endpointApi) of
                        (UserKey (Just userId'), ApiRegistrationKey (Just apiId')) ->
                            ScopeContextApi (userId' ^. re userIdUuid) (apiId' ^. re apiIdUuid)
                        (UserKey (Just userId'), _) ->
                            ScopeContextUser (userId' ^. re userIdUuid)
                        _ -> ScopeContextGlobal
              apiNm = ApiName $ endpoint' ^. endpointPlugin
              reqNm = Identifier $ endpoint' ^. endpointRequest
          hapiReq <- mkHapiRequest 4096 req
          res <- runExceptT $ do
                   let value = encodeHapiValue hapiReq
                   args' <- ExceptT $ requestFromPlugin mgr apiNm (PluginRequestFill scope reqNm (HapiObject (HM.singleton "req" value)))
                   -- TODO support asynchronous calls here
                   (_, val) <- ExceptT $ requestFromPlugin mgr apiNm (PluginRequestPerform scope (reqNm ^. _Identifier . re _ApiRequestName) args')
                   case val of
                     HapiValueObject (TypeName (Just "com.hapi") "Response") res ->
                       case runHapiParser $ decodeHapiObject res of
                         Left err -> throwError (PluginError "com.hapi" ("Couldn't decode response: " <> fromString err))
                         Right res' -> pure res'
                     _ -> throwError (PluginError "com.hapi" "Expected object in response to request")
          case res of
            Left PluginAsync {} -> resp (responseLBS status500 [] "Asynchronous")
            Left (PluginError _ err) -> resp (responseLBS status500 [] (fromString (T.unpack err)))
            Right httpResponse -> resp (fromHapiResponse httpResponse)

    respExceptT resp sts =
      withExceptT (\e -> resp (responseLBS sts [] (BS.fromStrict $ TE.encodeUtf8 e)))

    doContinuation asyncTok _ resp =
      join . fmap (either id id) .
      runExceptT $ do
        (_, asyncTok') <-
          respExceptT resp status400 $
          decodeAsyncToken (mgr ^. pluginManagerConfig) Nothing asyncTok
        ExceptT $ lookupAsyncInfo mgr asyncTok' (\err -> Left (resp (responseLBS status404 [] (BS.fromStrict (TE.encodeUtf8 err))))) $ \asyncInfo ->
          runExceptT $ do
          ourName <- liftIO getHostName
          if asyncInfo ^. asyncInfoLocation /= fromString ourName
            then do
              let newLoc = TE.encodeUtf8 ("//" <> asyncInfo ^. asyncInfoLocation <> (mgr ^. pluginManagerAsyncContinueLink) asyncTok)
              pure (resp (responseLBS status303 [("Location", newLoc)] ""))
            else do
              waitForNextResponse <- ExceptT . atomically $ do
                asyncs <- readTVar (mgr ^. pluginManagerAsyncs)
                case asyncs ^. at asyncTok' of
                  Nothing -> pure (Left (resp (responseLBS status404 [] "Not found")))
                  Just (_, saveVar, completeVar, waitForNextResponseVar) -> do
                    intermediateRes <- readTMVar saveVar
                    putTMVar completeVar intermediateRes
                    modifyTVar' (mgr ^. pluginManagerAsyncs) (at asyncTok' .~ Nothing)
                    Right <$> takeTMVar waitForNextResponseVar

              nextResult <- respExceptT resp status500 $
                            withExceptT (\e -> case e of
                                                 PluginAsync {} -> "Unexpected async"
                                                 PluginError _ e' -> e') $
                            ExceptT waitForNextResponse
              case nextResult of
                AsyncResponseContinues timeout token tyName url doNext -> do
                  hdrs <- liftIO $ setupAsync mgr timeout token tyName url doNext
                  pure (resp (responseLBS (mkStatus 303 "Asynchronous")
                                          hdrs "{}"))
                AsyncResponseDone (_, val) ->
                  pure (resp (responseLBS status200 [] (JSON.encode val)))


hapiServer :: FilePath -> P.Pool Db.Connection -> PluginManager -> Server HapiAPI
hapiServer resourcePath pool mgr =
  Tagged (staticApp (defaultWebAppSettings resourcePath)) :<|>
  createUser :<|> userServer pool :<|>
  apiServer pool mgr :<|>
  describeApiServer mgr :<|>
  publicEndpoint pool mgr :<|>
  apiEndpoint pool mgr :<|>
  Tagged (fileUploadEndpoint pool (mgr ^. pluginManagerFileBackend))
  where
    createUser userCreation = do
      [ dbUser ] <- liftIO $ P.withResource pool $ flip withDatabase $
                    runInsertReturningList (hapiDb ^. dbUsers) $
                    insertExpressions [ Db.User default_ (val_ (userCreation ^. userCreationHomepage)) default_ ]
      pure (User (review userIdUuid (dbUser ^. Db.userId)) (dbUser ^. Db.userHomepage))

hapiWebApp :: FilePath -> P.Pool Db.Connection -> PluginManager -> Application
hapiWebApp resourcePath dbPool mgr = serve hapiApi (hapiServer resourcePath dbPool mgr)

main :: IO ()
main = do
  HapiOptions { port = portNo
              , config = configFile
              , migrate = doMigrate } <-
    getRecord "Hapi server"

  configData <- readHapiConfig (unHelpful configFile)

  case configData of
    Nothing -> fail "Could not read config"
    Just configData'
      | unHelpful doMigrate -> do
          putStrLn (backendMigrationScript (BS.unpack . Pg.pgRenderSyntaxScript . Pg.fromPgCommand) migration)
      | otherwise -> do
          putStrLn "Starting hapi"

          pool <- newDbPoolFromConfig configData'
          registry <- crossLinkRegistry globalLinkifier <$>
                      registryFromConfig configData'
          let endpointLink endpointNm =
                fromString ('/':show (linkURI (safeLink hapiApi publicEndpointLink endpointNm)))
              continueLink (AsyncToken tok) =
                endpointLink ("cont-" <> tok)
          mgr <- newPluginManager configData' registry endpointLink continueLink
                                  (\fn -> P.withResource pool $ flip withDatabase fn)
                                  (localFileBackend "uploads")

          run (unHelpful portNo) (hapiWebApp "./resources/" pool mgr)
