{-# OPTIONS_GHC -fno-warn-orphans #-}
module HapiAPI where

import           Web.Hapi.Types
import           Web.Hapi.Registry
import           Web.Hapi.Plugin.Idl hiding (Value)

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Applicative

import           Data.Aeson (FromJSON(..), ToJSON(..), (.:), withObject, encode)
import           Data.String
import           Data.Maybe (maybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Proxy
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.URL.Lazy as B64

import           Lucid

import           Servant
import           Servant.HTML.Lucid

import           Web.FormUrlEncoded as Form

newtype ApiCreationPayload = ApiCreationPayload ApiName
newtype ApiRequestInstanceDescription = ApiRequestInstanceDescription ApiSettingsDescription

instance FromJSON ApiCreationPayload where
  parseJSON = withObject "ApiCreationPayload" $ \o ->
              ApiCreationPayload <$> o .: "apiName"
instance FromForm ApiCreationPayload where
  fromForm f = ApiCreationPayload . ApiName <$> Form.parseUnique "apiName" f

instance ToJSON ApiRequestInstanceDescription where
  toJSON (ApiRequestInstanceDescription d) = toJSON d

data JSONFormData
instance Accept JSONFormData where
  contentType _ = contentType (Proxy :: Proxy FormUrlEncoded)
instance FromJSON a => MimeUnrender JSONFormData a where
  mimeUnrender _ bs = do
    (param, val) <- pure (BL.break (=='=') bs)
    guard (param == "POST_DATA" && not (BL.null val))
    let val' = BL.map (\c -> if c == '*' then '=' else c) (BL.tail val)
    unparsedData <- B64.decode val'
    mimeUnrender (Proxy :: Proxy JSON) unparsedData

type UserAPI = Get '[JSON] User :<|>
               ReqBody '[JSON] User :> Put '[JSON] User :<|>
               Delete '[JSON] ()

type ApiInstanceAPI = Get '[JSON, HTML] ActivatedApi :<|>
                      "lookup" :> Capture "lookup-name" Identifier :> "description" :> LookupDescriptionAPI :<|>
                      "settings" :> Get '[JSON, HTML] ApiSettingsDescription :<|>
                      "settings" :> ReqBody '[JSON] HapiObject :> Put '[JSON, HTML] ApiSettingsDescription :<|>
                      Delete '[JSON] ()

type ApiAPI = Get '[JSON, HTML] ApisStatus :<|>
              ReqBody '[JSON, FormUrlEncoded] ApiCreationPayload :> Post '[JSON] () :<|>
              Capture "api-id" ApiId :> ApiInstanceAPI :<|>
              Capture "api-name" ApiName :> ApiDescriptionAPI

type ApiDescriptionAPI = Get '[JSON, HTML] ApiDescription :<|>
                         "lookup" :> Capture "lookup-name" Identifier :> LookupDescriptionAPI

type LookupDescriptionAPI = Get '[JSON] ApiLookupDescription

type RequestAPI = Get '[JSON, HTML] ApiRequestInstanceDescription :<|>
                  ReqBody '[JSON, JSONFormData] HapiObject :> Post '[JSON] HapiValue

type HapiAPI = "resources" :> Raw                                          :<|>

               "user" :> ReqBody '[JSON] UserCreation :> Post '[JSON] User :<|>
               "user" :> Capture "user-id" UserId :> UserAPI               :<|>

               "user" :> Capture "user-id" UserId :> "api" :> ApiAPI       :<|>
               "api"  :> Capture "api-name" ApiName :> ApiDescriptionAPI   :<|>

               "public" :> Capture "endpoint-name" Text :> Raw             :<|>

               "user" :> Capture "user-id" UserId :> "api" :> Capture "api-id" ApiId
                      :> Capture "request" ApiRequestName
                      :> RequestAPI :<|>
               "files" :> Raw

hapiApi :: Proxy HapiAPI
hapiApi = Proxy

publicEndpointLink :: Proxy ("public" :> Capture "endpoint-name" Text :> Raw)
publicEndpointLink = Proxy

getApiLink :: Proxy ("user" :> Capture "user-id" UserId :> "api" :> Capture "api-id" ApiId :> Get '[JSON, HTML] ActivatedApi)
getApiLink = Proxy

getApiSettingsLink :: Proxy ("user" :> Capture "user-id" UserId :> "api" :> Capture "api-id" ApiId :> "settings" :> Get '[JSON] ApiSettingsDescription)
getApiSettingsLink = Proxy

describeApiLink :: Proxy ("api" :> Capture "api-name" ApiName :> Get '[JSON] ApiDescription)
describeApiLink = Proxy

describeApiLookupLink :: Proxy ("user" :> Capture "user-id" UserId :> "api" :> Capture "api-id" ApiId :> "lookup" :> Capture "lookup-name" Identifier :> "description" :> Get '[JSON] ApiLookupDescription)
describeApiLookupLink = Proxy

requestLink :: Proxy ("user" :> Capture "user-id" UserId :> "api" :> Capture "api-id" ApiId :> Capture "request" ApiRequestName :> Get '[JSON, HTML] ApiRequestInstanceDescription)
requestLink = Proxy

describeUserApiLink :: Proxy ("user" :> Capture "user-id" UserId :> "api" :> Capture "api-name" ApiName :> Get '[JSON, HTML] ApiDescription)
describeUserApiLink = Proxy

createUserApiLink :: Proxy ("user" :> Capture "user-id" UserId :> "api" :> ReqBody '[JSON, FormUrlEncoded] ApiCreationPayload :> Post '[JSON] ())
createUserApiLink = Proxy

describeLookupLink :: Proxy ("api" :> Capture "api-name" ApiName :> "lookup" :> Capture "lookup-name" Identifier :> Get '[JSON] ApiLookupDescription)
describeLookupLink = Proxy

describeUserLookupLink :: Proxy ("user" :> Capture "user-id" UserId :> "api" :> Capture "api-name" ApiName :> "lookup" :> Capture "lookup-name" Identifier :> Get '[JSON] ApiLookupDescription)
describeUserLookupLink = Proxy

globalLinkifier :: ApiLinkifier
globalLinkifier =
  ApiLinkifier
  { linkLookupDescriptionUrl = \pkg l ->
      case l ^. apiLookupScope of
        ApiScopeGlobal -> Just (linkURI (safeLink hapiApi describeLookupLink pkg (l ^. apiLookupName)))
        _ -> Nothing
  , linkLookupUrl = \_ _ -> Nothing
  , linkTypeDescriptionUrl = \_ _ -> Nothing
  , linkPackageUrl = \_ -> Nothing
  , linkRequestUrl = \_ _ -> Nothing
  }

userLinkifier :: UserId -> ApiLinkifier
userLinkifier uid =
  globalLinkifier
  { linkLookupDescriptionUrl =
      \pkg l -> linkLookupDescriptionUrl globalLinkifier pkg l <|>
                case l ^. apiLookupScope of
                  ApiScopeUser -> Just (linkURI (safeLink hapiApi describeUserLookupLink uid pkg (l ^. apiLookupName)))
                  _ -> Nothing
  }

apiLinkifier :: UserId -> ApiId -> ApiLinkifier
apiLinkifier uid apiId' =
  let linkifier = userLinkifier uid
  in linkifier
  { linkLookupDescriptionUrl =
      \pkg l -> linkLookupDescriptionUrl linkifier pkg l <|>
                case l ^. apiLookupScope of
                  ApiScopeInstance -> Just (linkURI (safeLink hapiApi describeApiLookupLink uid apiId' (l ^. apiLookupName)))
                  _ -> Nothing
  , linkRequestUrl =
      \_ r ->
        Just (linkURI (safeLink hapiApi requestLink uid apiId' (r ^. apiRequestName)))
  }

-- * Orphans

glyphicon_ :: Monad m => Text -> HtmlT m ()
glyphicon_ nm = span_ [class_ ("glyphicons glyphicons-" <> nm)] mempty

mkLink :: URI -> Text
mkLink = fromString . ('/':) . show

resources_ :: Monad m => HtmlT m ()
resources_ = do
  link_ [ href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css", type_ "text/css"
        , rel_ "stylesheet" ]
  link_ [ href_ "/resources/form.css", type_ "text/css"
        , rel_ "stylesheet" ]
  script_ [ src_ "https://code.jquery.com/jquery-3.2.1.min.js"
          , type_ "text/javascript"
          , lang_ "javascript" ] ("" :: Text)
  script_ [ src_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
          , type_ "text/javascript"
          , lang_ "javascript" ] ("" :: Text)
  script_ [ src_ "/resources/form.js"
          , type_ "text/javascript"
          , lang_ "javascript" ] ("" :: Text)

apiInformation :: Monad m => ApiDescription -> HtmlT m ()
apiInformation descr = do
  section_ [ id_ "api-lookups" ] $ do
    h2_ "Lookups"
    div_ [class_ "list-group"] $
      forM_ (descr ^. apiDescriptionLookups) $ \lookup' ->
        let nm = toHtml $ lookup' ^. apiLookupName . _Identifier
        in case lookup' ^. apiLookupDescriptionUrl of
             Nothing -> a_ [class_ "list-group-item", href_ "#"] nm
             Just href -> a_ [class_ "list-group-item", href_ (fromString ('/':show href))] nm
  section_ [ id_ "api-requests" ] $ do
    h2_ "Api Requests"
    div_ [class_ "list-group"] $
      forM_ (descr ^. apiDescriptionRequests) $ \request' ->
        let nm = toHtml $ request' ^. apiRequestName . _ApiRequestName
        in case request' ^. apiRequestUrl of
          Nothing -> a_ [class_ "list-group-item", href_ "#"] nm
          Just href -> a_ [class_ "list-group-item", href_ (fromString ('/':show href))] nm

instance ToHtml ApiDescription where
  toHtmlRaw = toHtml
  toHtml descr =
    doctypehtml_ $ do
      head_ $ do
        title_ (toHtml (descr ^. apiDescriptionName . _ApiName))
        resources_
      body_ $ do
        h1_ (toHtml (descr ^. apiDescriptionName . _ApiName))
        case descr ^. apiDescriptionCreateUrl of
          Nothing -> pure ()
          Just createUrl ->
            form_ [ method_ "POST", action_ (fromString ('/':show createUrl)) ] $ do
              input_ [ type_ "hidden", name_ "apiName", value_ (descr ^. apiDescriptionName . _ApiName)]
              button_ [ class_ "btn btn-primary", type_ "submit" ] "Create API Instance for this user"
        apiInformation descr

instance ToHtml ApisStatus where
  toHtmlRaw = toHtml
  toHtml sts =
    doctypehtml_ $ do
      head_ $ do
        title_ "API status"
        resources_
      body_ $ do
        h1_ "API status"
        section_ [ id_ "active-apis" ] $ do
          h2_ "Active APIs"
          div_ [class_ "list-group"]$
            forM_ (sts ^. apisStatusActive) $ \active ->
              a_ [class_ "list-group-item", href_ (fromString ('/':show (active ^. activeApiLinkUrl)))] (toHtml (active ^. activeApiLinkId . _ApiId))
        section_ [ id_ "available-apis" ] $ do
          h2_ "Available APIs"
          ul_ [class_ "list-group"] $
            forM_ (sts ^. apisStatusAvailable) $ \available ->
              li_ [class_ "list-group-item" ]$ do
                a_ [href_ (fromString ('/':show (available ^. availableApiLinkDescriptionUrl)))] (toHtml (available ^. availableApiLinkName . _ApiName))
                " "
                a_ [href_ (fromString ('/':show (available ^. availableApiLinkCreateUrl)))] "Create"

instance ToHtml ActivatedApi where
  toHtmlRaw = toHtml
  toHtml api =
    doctypehtml_ $ do
    head_ $ do
      title_ "API"
      resources_
    body_ $ do
      h1_ ("API " >> toHtml (api ^. activatedApiId . _ApiId))
      div_ $ do
        a_ [ class_ "label label-default"
           , href_ (maybe "#" mkLink (api ^. activatedApiApiUrl)) ] $ do
          glyphicon_ "cluster"
          toHtml (api ^. activatedApiApi . _ApiName)
        " "
        span_ [ class_ $
                case api ^. activatedApiStatus of
                  ApiStatusUp -> "label label-success"
                  ApiStatusError {} -> "label label-danger" ] $ do
          "Status: "
          case api ^. activatedApiStatus of
            ApiStatusUp -> "Up"
            ApiStatusError {} -> "Error"
      case api ^. activatedApiSettingsUrl of
        Nothing -> mempty
        Just url ->
          a_ [ href_ (fromString ('/':show url)) ] $
          button_ [ class_ "btn btn-primary" ] "Settings"

      apiInformation (api ^. activatedApiDescription)

settingsForm :: Monad m => Text -> ApiSettingsDescription -> HtmlT m ()
settingsForm method api = do
  case api ^. apiSettingsErrors of
    [] -> pure ()
    apiErrors ->
      div_ [ class_ "alert alert-danger" ] $ do
        p_ "This API reported the following errors"
        ul_ (mapM_ (li_ . toHtml) apiErrors)

  let jsonAttr :: ToJSON a => a -> Text
      jsonAttr = TE.decodeUtf8 . BL.toStrict . B64.encode . encode

      settingsSection :: Monad m => T.Text -> ApiSettingsSection -> HtmlT m ()
      settingsSection prefix section = do
        case section ^. apiSettingsSectionHelp of
          Nothing -> mempty
          Just help ->
            div_ [class_ "alert alert-info"] (toHtmlRaw help)
        forM_ (section ^. apiSettingsSectionFields) (settingsField prefix)

      settingsField :: Monad m => T.Text -> ApiSettingsField -> HtmlT m ()
      settingsField prefix field =
        case field ^. apiSettingsFieldControl of
          ApiSettingsControlLabel l ->
            formControl field $
            input_ [ type_ "text"
                   , class_ "form-control"
                   , term "data-hapi-path" (prefix <> field ^. apiSettingsFieldName)
                   , value_ l
                   , disabled_ "true" ]
          ApiSettingsControlText t ->
            formControl field $
            input_ [ type_ "text"
                   , class_ "form-control"
                   , term "data-hapi-path" (prefix <> field ^. apiSettingsFieldName)
                   , value_ t ]
          ApiSettingsControlNumber False (Nothing, Nothing) v ->
            formControl field $
            input_ [ type_ "text"
                   , class_ "form-control"
                   , term "data-hapi-path" (prefix <> field ^. apiSettingsFieldName)
                   , value_ (fromString (show (floor v :: Integer)))
                   , term "data-hapi-type" "number" ]
          ApiSettingsControlFileUpload t ->
            formControl field $
            div_ ([ class_ "hapi-file-upload"
                  , term "data-hapi-file-upload-backend-url" "/files/"
                  , term "data-hapi-type" "file-upload"
                  , term "data-hapi-path" (prefix <> field ^. apiSettingsFieldName) ] ++
                  case t of
                    Nothing -> []
                    Just (nm, uri) ->
                      [ term "data-hapi-file-upload-name" nm
                      , term "data-hapi-file-upload-url" uri ]) mempty
          ApiSettingsControlCheckBox checked ->
            div_ [class_ "checkbox"] $
              label_ $ do
                input_ ([ type_ "checkbox"
                        , term "data-hapi-path" (prefix <> field ^. apiSettingsFieldName)
                        , term "data-hapi-type" "switch" ] ++
                         if checked then [ checked_ ] else [] )
                strong_ $
                  toHtml (field ^. apiSettingsFieldLabel)
                case field ^. apiSettingsFieldHelp of
                  Nothing -> pure ()
                  Just help -> do
                    " -- "
                    toHtmlRaw help
          ApiSettingsControlChoices False choices selection ->
            formControl field $
            select_ [ class_ "form-control"
                    , term "data-hapi-path" (prefix <> field ^. apiSettingsFieldName)
                    , term "data-hapi-type" "choices" ] $
              forM_ (zip [0..] choices) $ \(i, (nm, choice)) ->
                option_ ([ value_ (jsonAttr choice) ] ++
                         if selection == i then [ selected_ "selected" ] else []) $
                toHtml nm
          ApiSettingsControlMulti ty d ->
            div_ [ class_ "hapi-sub-form panel panel-default"
                 , term "data-hapi-type" "multi"
                 , term "data-hapi-type-name" (jsonAttr ty)
                 , term "data-hapi-path" (prefix <> field ^. apiSettingsFieldName) ] $ do
              div_ [class_ "panel-heading"] (toHtml (field ^. apiSettingsFieldLabel))
              div_ [class_ "panel-body"] $ do
                case d ^. apiSettingsErrors of
                  [] -> pure ()
                  fieldErrors ->
                    div_ [class_ "alert alert-danger"] $
                    ul_ (mapM_ (li_ . toHtml) fieldErrors)
                case field ^. apiSettingsFieldHelp of
                  Nothing -> pure ()
                  Just help -> div_ [class_ "alert alert-success"] (toHtmlRaw help)
                let prefix' = prefix <> field ^. apiSettingsFieldName <> ".data."
                settingsSection prefix' (d ^. apiSettingsFields)
                forM_ (d ^. apiSettingsSections) $ \(nm, section) ->
                  fieldset_ $ do
                    legend_ (toHtml nm)
                    settingsSection prefix' section
          ctl -> toHtml (show ctl)

      formControl :: Monad m => ApiSettingsField -> HtmlT m () -> HtmlT m ()
      formControl field x =
        div_ [class_ "form-group"] $ do
          label_ [for_ (field ^. apiSettingsFieldName)] $ toHtml (field ^. apiSettingsFieldLabel)
          x
          case field ^. apiSettingsFieldHelp of
            Nothing -> mempty
            Just help -> div_ [class_ "form-text"] (toHtmlRaw help)

  form_ [ term "data-hapi-form" "true"
        , term "data-hapi-method" method ]$ do
    settingsSection "" (api ^. apiSettingsFields)
    forM_ (api ^. apiSettingsSections) $ \(nm, section) ->
      fieldset_ $ do
        legend_ (toHtml nm)
        settingsSection "" section

    button_ [ class_ "btn btn-primary"
            , term "onclick" "javascript:submitHapiForm()" ]
            "Submit"


instance ToHtml ApiSettingsDescription where
  toHtmlRaw = toHtml
  toHtml api =
    doctypehtml_ $ do
    head_ $ do
      title_ "API settings"
      resources_
    body_ $ do
      h1_ "API Settings"

      settingsForm "PUTSETTINGS" api

instance ToHtml ApiRequestInstanceDescription where
  toHtmlRaw = toHtml
  toHtml (ApiRequestInstanceDescription api) =
    doctypehtml_ $ do
    head_ $ do
      title_ "Request"
      resources_
    body_ $ do
      h1_ "Request"

      settingsForm "HTMLPOST" api

-- * Utilities

class HandleAll server where
  throwAll :: ServantErr -> server

instance HandleAll (Handler a) where
  throwAll = throwError
instance (HandleAll a, HandleAll b) => HandleAll (a :<|> b) where
  throwAll e = throwAll e :<|> throwAll e
instance HandleAll b => HandleAll (a -> b) where
  throwAll e _ = throwAll e

redirect303 :: Link -> Handler a
redirect303 url = throwError err303 { errHeaders = [("Location", fromString ('/':show (linkURI url)))] }
