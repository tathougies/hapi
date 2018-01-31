module Web.Hapi.GitHub
  ( req_Configure
  , req_ListRepositories
  , req_UserCallback ) where

import           Web.Hapi.GitHub.Types
import           Web.Hapi.Plugin.Monad
import           Web.Hapi.Plugin.Interface
import           Web.Hapi.Types

import           Control.Applicative
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Aeson (decode, encode, object, (.=))
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List (intercalate)
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX
import qualified Data.Vector as V
import           Data.Word

import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP


import           System.IO

apiRoot :: String
apiRoot = "https://api.github.com/"

req_Configure :: Args'Configure -> PluginM [T.Text]
req_Configure (Args'Configure (Data'GitHubConfiguration clientId clientSecret _ _)) =
  let errors = (do { "" <- pure clientId ; pure "Please set the clientId" }) <|>
               (do { "" <- pure clientSecret ; pure "Please set the clientId" })
  in pure errors

githubScopes :: Data'GitHubPermissions -> String
githubScopes (Data'GitHubPermissions user publicRepo repo repoDeployment deleteRepo notifications
                                     gist repoHook orgHook org publicKey gpgKey) =
  intercalate "%20" . mconcat $
  [ user ==> "user", publicRepo ==> "public_repo", repo ==> "repo"
  , repoDeployment ==> "repo_deployment", deleteRepo ==> "delete_repo"
  , notifications ==> "notifications", gist ==> "gist"
  , accessToken repoHook "repo_hook"
  , orgHook ==> "admin:org_hook"
  , accessToken org "org"
  , accessToken publicKey "public_key"
  , accessToken gpgKey "gpg_key"
  ]
  where
    accessToken Data'GitHubAccess_None  _  = []
    accessToken Data'GitHubAccess_Read  nm = ["read:" ++ nm]
    accessToken Data'GitHubAccess_Write nm = ["write:" ++ nm]
    accessToken Data'GitHubAccess_Admin nm = ["admin:" ++ nm]

    True ==> x = pure x
    False ==> _ = mempty

githubUserToken :: Data'GitHubConfiguration -> T.Text -> PluginM BS.ByteString
githubUserToken (Data'GitHubConfiguration clientId clientSecret _ scopes) username = do
  let key = Key'GitHubUserTokens (Data'GitHubUser username)

  val <- pluginLookup lookup_GitHubUserTokens key
  case val of
    Just (Value'GitHubUserTokens (Data'GitHubUserToken token)) ->
      pure (TE.encodeUtf8 token)
    Nothing -> do
      secret <- liftIO randomString
      Data'GitHubAuthCode code  secret' <-
        asynchronous (10 * 60) $ \(AsyncToken asyncToken) ->
        "http://github.com/login/oauth/authorize?client_id=" ++ T.unpack clientId ++
        "&state=" ++ T.unpack secret ++ "." ++ T.unpack asyncToken ++
        "&scope=" ++ githubScopes scopes

      when (secret /= secret') $
        fail "Spoofing attack detected"

      initTokenReq <- HTTP.parseRequest ("https://github.com/login/oauth/access_token" ++
                                         "?client_id=" ++ T.unpack clientId ++
                                         "&client_secret=" ++ T.unpack clientSecret ++
                                         "&code=" ++ T.unpack code)
      let tokenReq = initTokenReq { HTTP.requestHeaders = [ ("User-Agent", "radiant-db")
                                                          , ("Accept", "application/json") ]
                                  , HTTP.method = HTTP.methodPost }

      res <- fmap fromHttpResponseJSON <$> httpReq tokenReq (== 200)
      case res of
        Left _ -> fail "Could not parse token response"
        Right (Data'GitHubAuthTokenResponse accessToken tokenType)
          | tokenType == "bearer" -> do
              pluginLookupSet lookup_GitHubUserTokens key (Value'GitHubUserTokens (Data'GitHubUserToken accessToken))
              pure (TE.encodeUtf8 accessToken)
          | otherwise -> fail ("Invalid token type: " ++ show tokenType)

req_ListRepositories :: Args'ListRepositories -> PluginM [Data'GitHubRepository]
req_ListRepositories (Args'ListRepositories cfg userKey asUser _ _) = do
  userToken <- githubUserToken cfg userKey

  initReq <- case asUser of
               Nothing -> HTTP.parseRequest (apiRoot ++ "user/repos")
               Just asUser' -> HTTP.parseRequest (apiRoot ++ "user/" ++ T.unpack asUser' ++ "/repos")
  liftIO $ hPutStrLn stderr ("Token is " ++ show userToken)
  let req = initReq { HTTP.requestHeaders = [ ("Accept", "application/vnd.github.machine-man-preview+json")
                                            , ("Authorization", "Token " <> userToken)
                                            , ("User-Agent", "radiant-db") ] }

  resp <- fmap fromHttpResponseJSON <$> httpReq req (==200)

  case resp of
    Left err -> do
      liftIO $ BL.hPutStrLn stderr (HTTP.responseBody err)
      fail ("Could not complete request: " ++ show (HTTP.responseStatus err))
    Right repos -> pure repos

errorResponse :: T.Text -> HapiHttpResponse
errorResponse msg = HapiHttpResponse 400 "Bad Request" msg []

redirectResponse :: T.Text -> HapiHttpResponse
redirectResponse url = HapiHttpResponse 303 "See Other" "" [ ("Location", url) ]

req_UserCallback :: Args'UserCallback -> PluginM HapiHttpResponse
req_UserCallback (Args'UserCallback req) =
  case ( lookup "code" (_httpRequestGetParams req)
       , lookup "state" (_httpRequestGetParams req) ) of
    ( Just (Just code), Just (Just secret) ) -> do
      liftIO $ hPutStrLn stderr ("Restarting " ++ show (code, secret))
      let (secret', asyncToken) = T.break (=='.') secret
      if T.null asyncToken then pure (errorResponse "Bad state")
        else do
          url <- kickAsync (AsyncToken (T.tail asyncToken)) (Data'GitHubAuthCode code secret')
          pure (redirectResponse url)
    _ -> pure (errorResponse "Missing code and secret")
