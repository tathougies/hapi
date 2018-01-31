{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Hapi.Types where

import           Web.Hapi.Plugin.Idl hiding (Value)

import           Control.Applicative
import           Control.Lens hiding ((.=))
import           Control.Monad

import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import qualified Data.CaseInsensitive as CI
import           Data.Char
import           Data.Data
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.Monoid
import           Data.Random.Source
import           Data.Random.Source.IO ()
import           Data.Scientific (Scientific)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Lens
import qualified Data.Text.Read as T
import           Data.Time (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID

import           GHC.Generics hiding (to)

import           Network.URI (URI, parseURI, uriAuthority, uriRegName)
import qualified Network.HTTP.Types as Wai (Status(..))
import qualified Network.Wai as Wai
import           Numeric

import           Servant

newtype UserId = UserId { userIdToken :: Text }
  deriving (Show, Eq, Ord, ToHttpApiData, Data)
newtype ApiId = ApiId { apiIdToken :: Text }
  deriving (Show, Eq, Ord, ToHttpApiData, Data)
newtype FileId = FileId { fileIdToken :: Text }
  deriving (Show, Eq, Ord, ToHttpApiData, Data)
newtype FileUrl = FileUrl { fileUrlText :: Text }
  deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype AsyncToken = AsyncToken { asyncTokenText :: Text }
  deriving (Show, Eq, Ord, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Hashable)
newtype ApiName = ApiName { apiName :: Text }
  deriving (Show, Eq, Ord, Hashable, ToJSONKey, FromJSONKey, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Generic, Data, IsString)
newtype PropertyName = PropertyName { propertyName :: Text }
  deriving (Show, Eq, Ord, Hashable, ToJSONKey, FromJSONKey, Data, IsString)
newtype ApiRequestName = ApiRequestName { getApiRequestName :: Text }
  deriving (Show, Eq, Ord, ToJSONKey, FromJSONKey, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, Generic, IsString, Data)

data HapiLookupKey
  = HapiLookupKey (Maybe UserId) (Maybe ApiId) Value
  deriving (Show, Eq, Generic)
instance ToJSON HapiLookupKey
instance FromJSON HapiLookupKey

data HapiAsyncInfo
  = HapiAsyncInfo
  { _asyncInfoLocation   :: T.Text
  , _asyncInfoExpType    :: TypeName
  , _asyncInfoSecretHash :: T.Text
  } deriving (Show, Generic)
instance ToJSON HapiAsyncInfo
instance FromJSON HapiAsyncInfo

data ScopeContext
  = ScopeContextGlobal
  | ScopeContextUser { _scopeContextUser :: !UserId }
  | ScopeContextApi  { _scopeContextUser :: !UserId, _scopeContextApi :: !ApiId }
  deriving (Show, Eq, Data)

data AsyncTokenInternal
  = AsyncTokenInternal
  { _asyncTokenInternal :: AsyncToken
  , _asyncTokenSecret   :: Text
  } deriving (Show, Eq)

data HapiHttpRequest
  = HapiHttpRequest
  { _httpRequestMethod    :: Text
  , _httpRequestPath      :: [Text]
  , _httpRequestIsSecure  :: Bool
  , _httpRequestGetParams :: [(Text, Maybe Text)]
  , _httpRequestBody      :: Text
  , _httpRequestHeaders   :: [(Text, Text)]
  } deriving (Show, Eq)

data HapiHttpResponse
  = HapiHttpResponse
  { _httpResponseStatus :: Int
  , _httpResponsePhrase :: Text
  , _httpResponseBody   :: Text
  , _httpResponseHeaders :: [ (Text, Text) ]
  } deriving (Show, Eq)

data HapiHttpResponseBody
  = HapiHttpResponseBodyEncoded HapiValue
  | HapiHttpResponseBodyBytes   ByteString
  deriving (Show, Eq)

newtype HapiObject
  = HapiObject
  { objectKeys  :: HM.HashMap PropertyName HapiValue
  } deriving (Show, Eq, Data)

data HapiValue
  = HapiValueText       !Text
  | HapiValueHtml       !Text
  | HapiValueNumber     !Scientific
  | HapiValueTimestamp  !UTCTime
  | HapiValueTuple      [ HapiValue ]
  | HapiValueList       [ HapiValue ]
  | HapiValueFile       !Text {-^ URI -}
  | HapiValueObject     !TypeName !HapiObject
  | HapiValueCredential !ByteString
  | HapiValueSwitch     !Bool
  | HapiValueJust       !HapiValue
  | HapiValueNothing
  deriving (Show, Eq, Data)

data ApiStatus
  = ApiStatusUp
  | ApiStatusError [Text]
  deriving (Show, Generic, Data)

data ApiEntityScope
  = ApiScopeGlobal
  | ApiScopeUser
  | ApiScopeInstance
  deriving (Show, Generic, Data)

data ApiEntityVisibility
  = ApiVisibilityInternal
  | ApiVisibilityPrivate
  | ApiVisibilityPublic
  deriving (Show, Generic, Data, Eq)

data ApiDescription
  = ApiDescription
  { _apiDescriptionName     :: ApiName
  , _apiDescriptionLookups  :: [ ApiLookupDescription ]
  , _apiDescriptionRequests :: [ ApiRequestDescription ]
  , _apiDescriptionTypes    :: [ ApiTypeDescription ]
  , _apiDescriptionCreateUrl :: Maybe URI
  } deriving (Show, Generic, Data)

data ApiLookupDescription
  = ApiLookupDescription
  { _apiLookupName    :: Identifier
  , _apiLookupScope   :: ApiEntityScope
  , _apiLookupVisible :: ApiEntityVisibility
  , _apiLookupKey     :: ApiCompoundTypeSchema
  , _apiLookupValue   :: ApiCompoundTypeSchema
  , _apiLookupDescriptionUrl, _apiLookupUrl :: Maybe URI
  } deriving (Show, Generic, Data)

data ApiRequestDescription
  = ApiRequestDescription
  { _apiRequestName    :: ApiRequestName
  , _apiRequestScope   :: ApiEntityScope
  , _apiRequestVisible :: ApiEntityVisibility
  , _apiRequestInput   :: ApiCompoundTypeSchema
  , _apiRequestOutput  :: ApiTypeDescription
  , _apiRequestUrl     :: Maybe URI
  } deriving (Show, Generic, Data)

data ApiTypeDescription
  = ApiTypeMany !ApiTypeDescription
  | ApiTypeOptional !ApiTypeDescription
  | ApiTypeTuple [ApiTypeDescription]
  | ApiTypeDescription
  { _apiTypeName       :: Identifier
  , _apiTypeSource     :: ApiTypeSource
  , _apiTypeDescriptionUrl :: Maybe URI
  } deriving (Show, Generic, Data)

data ApiTypeSource
  = ApiTypeSourceBuiltin
  | ApiTypeSourcePackage
  { _apiTypeSourcePackage :: PackageName
  , _apiTypeSourcePackageUrl :: Maybe URI
  } deriving (Show, Generic, Data)

data ApiCompoundTypeSchema
  = ApiCompoundTypeSchema
  { _apiCompoundTypeFields :: [ApiCompoundFieldSchema]
  , _apiCompoundTypeSections :: [ (Identifier, Text, [ApiCompoundFieldSchema]) ]
  } deriving (Show, Generic, Data)

data ApiCompoundFieldSchema
  = ApiCompoundFieldSchema
  { _apiCompoundFieldName        :: Identifier
  , _apiCompoundFieldType        :: ApiTypeDescription
  , _apiCompoundFieldVisibility  :: ApiEntityVisibility
  , _apiCompoundFieldRequired    :: Bool
  , _apiCompoundFieldDescription :: Text
  } deriving (Show, Generic, Data)

data ApiTypeSchema
  = ApiTypeSchemaChoices [(Identifier, Maybe ApiTypeDescription)]
  | ApiTypeSchemaCompound ApiCompoundTypeSchema
  | ApiTypeSchemaOpaque Text
  deriving (Show, Generic, Data)

data ApisStatus
  = ApisStatus
  { _apisStatusAvailable :: [ AvailableApiLink ]
  , _apisStatusActive    :: [ ActiveApiLink ]
  } deriving (Show, Generic, Data)

data ActiveApiLink
  = ActiveApiLink
  { _activeApiLinkId  :: ApiId
  , _activeApiLinkUrl :: URI
  } deriving (Show, Generic, Data)

data AvailableApiLink
  = AvailableApiLink
  { _availableApiLinkName :: ApiName
  , _availableApiLinkDescriptionUrl :: URI
  , _availableApiLinkCreateUrl :: URI
  } deriving (Show, Generic, Data)

data User
  = User
  { _userId :: UserId
  , _userHomepage :: Text
  } deriving (Show, Generic, Data)

data UserCreation
  = UserCreation
  { _userCreationHomepage :: Text
  } deriving (Show, Generic, Data)

data ActivatedApi
  = ActivatedApi
  { _activatedApiId          :: ApiId
  , _activatedApiApi         :: ApiName
  , _activatedApiApiUrl      :: Maybe URI
  , _activatedApiStatus      :: ApiStatus
  , _activatedApiSettings    :: HapiObject
  , _activatedApiSettingsUrl :: Maybe URI

  , _activatedApiDescription :: ApiDescription
  } deriving (Show, Generic, Data)

data ApiSettingsDescription
  = ApiSettingsDescription
  { _apiSettingsSections :: [ (Text, ApiSettingsSection) ]
  , _apiSettingsFields   :: ApiSettingsSection
  , _apiSettingsErrors   :: [ Text ]
  } deriving (Show, Generic, Data)

data ApiSettingsSection
  = ApiSettingsSection
  { _apiSettingsSectionHelp :: Maybe Text
  , _apiSettingsSectionFields :: [ ApiSettingsField ]
  } deriving (Show, Generic, Data)

data ApiSettingsField
  = ApiSettingsField
  { _apiSettingsFieldName :: Text
  , _apiSettingsFieldLabel :: Text
  , _apiSettingsFieldHelp :: Maybe Text
  , _apiSettingsFieldControl :: ApiSettingsControl }
  deriving (Show, Generic, Data)

data ApiSettingsControl
  = ApiSettingsControlText Text
  | ApiSettingsControlCalendar UTCTime
  | ApiSettingsControlNumber Bool {-^ True if floating -} (Maybe Scientific, Maybe Scientific) Scientific
  | ApiSettingsControlChoices Bool {-^ True if custom allowed -} [ (Text, HapiValue) ] Int
  | ApiSettingsControlCheckBox Bool
  | ApiSettingsControlLabel Text
  | ApiSettingsControlFileUpload (Maybe (Text, Text))
  | ApiSettingsControlOptional Bool ApiSettingsControl
  | ApiSettingsControlMulti TypeName ApiSettingsDescription
  deriving (Show, Generic, Data)

makeLenses ''HapiAsyncInfo
makeLenses ''ScopeContext
makeLenses ''HapiHttpRequest
makeLenses ''HapiHttpResponse
makeLenses ''AsyncTokenInternal
makeLenses ''User
makeLenses ''UserCreation
makeLenses ''ApiTypeSource
makeLenses ''ActivatedApi
makeLenses ''AvailableApiLink
makeLenses ''ActiveApiLink
makeLenses ''ApiDescription
makeLenses ''ApiCompoundTypeSchema
makeLenses ''ApiCompoundFieldSchema
makeLenses ''ApiLookupDescription
makeLenses ''ApiRequestDescription
makeLenses ''ApiTypeDescription
makeLenses ''ApisStatus
makeLenses ''ApiSettingsDescription
makeLenses ''ApiSettingsSection
makeLenses ''ApiSettingsField

makePrisms ''ScopeContext
makePrisms ''AsyncToken
makePrisms ''ApiSettingsControl
makePrisms ''UserId
makePrisms ''FileId
makePrisms ''ApiId
makePrisms ''ApiName
makePrisms ''PropertyName
makePrisms ''ApiRequestName
makePrisms ''HapiObject
makePrisms ''HapiValue
makePrisms ''ApiStatus
makePrisms ''ApiTypeSchema

atScope :: ApiEntityScope -> Traversal' ScopeContext ScopeContext
atScope ApiScopeGlobal f _ = f ScopeContextGlobal
atScope ApiScopeUser _ ScopeContextGlobal = pure ScopeContextGlobal
atScope ApiScopeUser f s@ScopeContextUser {} = f s
atScope ApiScopeUser f (ScopeContextApi user _) = f (ScopeContextUser user)
atScope ApiScopeInstance _ ScopeContextGlobal = pure ScopeContextGlobal
atScope ApiScopeInstance _ s@ScopeContextUser {} = pure s
atScope ApiScopeInstance f s@ScopeContextApi {} = f s

instance FromHttpApiData UserId where
  parseUrlPiece txt =
    case txt ^? re _UserId . userIdUuid of
      Just {} -> Right (UserId txt)
      Nothing -> Left "Cannot parse UserId"
instance FromHttpApiData ApiId where
  parseUrlPiece txt =
    case txt ^? re _ApiId . apiIdUuid of
      Just {} -> Right (ApiId txt)
      Nothing -> Left "Cannot parse ApiId"

instance ToJSON User where
  toJSON (User userId' homepage) = object [ "id" .= userId', "homepage" .= homepage ]
instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User <$> v .: "id" <*> v .: "homepage"

instance FromJSON UserCreation where
  parseJSON = withObject "UserCreation" $ \v -> UserCreation <$> v .: "homepage"

hapiValueFiles :: Traversal' HapiValue Text
hapiValueFiles f (HapiValueTuple xs) = HapiValueTuple <$> traverse (hapiValueFiles f) xs
hapiValueFiles f (HapiValueList xs) = HapiValueList <$> traverse (hapiValueFiles f) xs
hapiValueFiles f (HapiValueObject ty (HapiObject d)) = HapiValueObject ty . HapiObject <$>
                                                       traverse (hapiValueFiles f) d
hapiValueFiles f (HapiValueJust x) = hapiValueFiles f x
hapiValueFiles f (HapiValueFile nm) = HapiValueFile <$> f nm
hapiValueFiles _ x = pure x

hapiObjectValues :: Traversal' HapiObject HapiValue
hapiObjectValues f (HapiObject xs) = HapiObject <$> traverse f xs

fileUrlFileId :: Prism' FileUrl FileId
fileUrlFileId =
  prism' (\(FileId fileId) -> FileUrl ("hapi-file://" <> fileId))
         (\(FileUrl url) ->
            do uri <- url ^? unpacked . to parseURI . _Just
               guard (uriScheme uri == "hapi-file:")
               uri ^? to uriAuthority . _Just . to uriRegName . packed . re _FileId)

detokenize :: Text -> Prism' Text UUID
detokenize pfx =
  prism' (\u -> let (a, b, c, d) = UUID.toWords u
                in pfx <> fromString (showHex a . showHex b . showHex c . showHex d $ ""))
         (\t -> do userIdData <- T.stripPrefix pfx t
                   [a, b, c, d] <- pure $ T.chunksOf 8 userIdData

                   guard (T.length d == 8)

                   Right (aHex, _) <- pure $ T.hexadecimal a
                   Right (bHex, _) <- pure $ T.hexadecimal b
                   Right (cHex, _) <- pure $ T.hexadecimal c
                   Right (dHex, _) <- pure $ T.hexadecimal d

                   pure (UUID.fromWords aHex bHex cHex dHex))

userIdUuid :: Prism' UserId UUID
userIdUuid = _UserId . detokenize "user"

apiIdUuid :: Prism' ApiId UUID
apiIdUuid = _ApiId . detokenize "api"

fileIdUuid :: Prism' FileId UUID
fileIdUuid = _FileId . detokenize "file"

randomString :: IO Text
randomString = do
  x <- getRandomNByteInteger 16
  let makeBase62 n
        | n >= 0  && n < 10 = chr (n + ord '0')
        | n >= 10 && n < 36 = chr (n - 10 + ord 'a')
        | n >= 36 && n < 62 = chr (n - 36 + ord 'A')
        | otherwise = 'X'
  pure (fromString (showIntAtBase 62 makeBase62 x ""))

instance ToJSON UserId where
  toJSON (UserId userId') = toJSON userId'
instance FromJSON UserId where
  parseJSON (String t)
    | "user" `T.isPrefixOf` t = pure (UserId t)
  parseJSON _ = fail "UserId"

instance ToJSON ApiId where
  toJSON (ApiId apiId') = toJSON apiId'
instance FromJSON ApiId where
  parseJSON (String t)
    | "api" `T.isPrefixOf` t = pure (ApiId t)
  parseJSON _ = fail "ApiId"

instance ToJSON ActivatedApi where
  toJSON (ActivatedApi apiId apiNm apiUrl sts settings settingsUrl desc) =
    object ([ "id" .= apiId
            , "apiName" .= apiNm
            , "status" .= sts
            , "settings" .= settings
            , "description" .= desc ] ++
            maybe mempty (\settingsUrl' -> [ "settingsUrl" .= ('/':show settingsUrl') ]) settingsUrl ++
            maybe mempty (\apiUrl' -> [ "apiUrl" .= ('/':show apiUrl') ]) apiUrl)

instance ToJSON ApiStatus where
  toJSON ApiStatusUp = "up"
  toJSON (ApiStatusError errorStr) = object [ "error" .= errorStr ]

instance ToJSON HapiObject where
  toJSON (HapiObject settings) = toJSON settings
instance FromJSON HapiObject where
  parseJSON x = HapiObject <$> parseJSON x

instance ToJSON HapiValue where
  toJSON (HapiValueText txt) = toJSON txt
  toJSON (HapiValueHtml html) = object [ "html" .= html ]
  toJSON (HapiValueNumber num) = toJSON num
  toJSON (HapiValueTimestamp utcTime) = toJSON utcTime
  toJSON (HapiValueTuple tuple) = object [ "tuple" .= tuple ]
  toJSON (HapiValueList list) = object [ "list" .= list ]
  toJSON (HapiValueFile file) = object [ "file" .= file ]
  toJSON HapiValueNothing = toJSON Null
  toJSON (HapiValueJust x) = object [ "just" .= x ]
  toJSON (HapiValueObject tyName obj) =
    object [ "$type" .= tyName
           , "data" .= obj ]
  toJSON (HapiValueCredential cred) = object [ "secret" .= TE.decodeUtf8 (B64.encode cred) ]
  toJSON (HapiValueSwitch b) = toJSON b
instance FromJSON HapiValue where
  parseJSON Null = pure HapiValueNothing
  parseJSON x = (HapiValueText <$> parseJSON x) <|>
                (HapiValueNumber <$> parseJSON x) <|>
                (HapiValueTimestamp <$> parseJSON x) <|>
                (HapiValueSwitch <$> parseJSON x) <|>
                (withObject "HapiValue" parseRest x)
    where
      parseRest o =
        HapiValueJust <$> o .: "just" <|>
        HapiValueTuple <$> o .: "tuple" <|>
        HapiValueList  <$> o .: "list"  <|>
        HapiValueFile  <$> o .: "file"  <|>
        HapiValueHtml  <$> o .: "html" <|>
        HapiValueObject <$> o .: "$type" <*> o .: "data" <|>
        parseCredential o

      parseCredential o = do
        String credTxt <- o .: "secret"
        let credBs = TE.encodeUtf8 credTxt
        case B64.decode credBs of
          Left decodeError -> fail ("ApiSetting.Credential: " <> decodeError)
          Right cred -> pure (HapiValueCredential cred)

instance ToJSON ApisStatus where
  toJSON (ApisStatus available active) =
    object [ "available" .= available
           , "active" .= active ]

instance ToJSON ActiveApiLink where
  toJSON (ActiveApiLink apiId link) =
    object [ "apiId" .= apiId
           , "apiUrl" .= ('/':show link) ]
instance ToJSON AvailableApiLink where
  toJSON (AvailableApiLink apiName' descriptionUrl createUrl) =
    object [ "name" .= apiName'
           , "descriptionUrl" .= ('/':show descriptionUrl)
           , "createUrl" .= ('/':show createUrl) ]

instance ToJSON ApiDescription where
  toJSON descr =
    object ([ "name" .= (descr ^. apiDescriptionName)
            , "lookups" .= (descr ^. apiDescriptionLookups)
            , "requests" .= (descr ^. apiDescriptionRequests)
            , "types" .= (descr ^. apiDescriptionTypes) ] ++
            maybe mempty (\createUrl' -> [ "createUrl" .= ('/':show createUrl') ]) (descr ^. apiDescriptionCreateUrl))
instance FromJSON ApiDescription where
  parseJSON = withObject "ApiDescription" $ \o ->
              ApiDescription <$> o .: "name" <*> o .: "lookups"
                             <*> o .: "requests" <*> o .: "types"
                             <*> o .:? "createUrl"

instance ToJSON ApiLookupDescription where
  toJSON descr =
    object ([ "name" .= (descr ^. apiLookupName)
            , "scope" .= (descr ^. apiLookupScope)
            , "visible" .= (descr ^. apiLookupVisible)
            , "key" .= (descr ^. apiLookupKey)
            , "value" .= (descr ^. apiLookupValue) ] ++
            maybe mempty (\descrUrl' -> [ "descriptionUrl" .= ('/':show descrUrl') ]) (descr ^. apiLookupDescriptionUrl) ++
            maybe mempty (\url' -> [ "lookupUrl" .= ('/':show url') ]) (descr ^. apiLookupUrl))
instance FromJSON ApiLookupDescription where
  parseJSON = withObject "ApiLookupDescription" $ \o ->
              ApiLookupDescription <$> o .: "name" <*> o .: "scope" <*> o .: "visible" <*> o .: "key" <*> o .: "value"
                                   <*> o .:? "descriptionUrl" <*> o .:? "lookupUrl"

instance ToJSON ApiEntityScope where
  toJSON ApiScopeGlobal = "global"
  toJSON ApiScopeUser = "user"
  toJSON ApiScopeInstance = "instance"
instance FromJSON ApiEntityScope where
  parseJSON "global" = pure ApiScopeGlobal
  parseJSON "user"   = pure ApiScopeUser
  parseJSON "instance" = pure ApiScopeInstance
  parseJSON _ = mzero

instance ToJSON ApiEntityVisibility where
  toJSON ApiVisibilityInternal = "internal"
  toJSON ApiVisibilityPrivate = "private"
  toJSON ApiVisibilityPublic = "public"
instance FromJSON ApiEntityVisibility where
  parseJSON "internal" = pure ApiVisibilityInternal
  parseJSON "private"  = pure ApiVisibilityPrivate
  parseJSON "public"   = pure ApiVisibilityPublic
  parseJSON _ = mzero

instance ToJSON ApiRequestDescription where
  toJSON descr =
    object ([ "name" .= (descr ^. apiRequestName)
            , "scope" .= (descr ^. apiRequestScope)
            , "visible" .= (descr ^. apiRequestVisible)
            , "input" .= (descr ^. apiRequestInput)
            , "output" .= (descr ^. apiRequestOutput) ] ++
            maybe mempty (\requestUrl' -> [ "requestUrl" .= ('/':show requestUrl') ]) (descr ^. apiRequestUrl))
instance FromJSON ApiRequestDescription where
  parseJSON = withObject "ApiRequestDescription" $ \o ->
              ApiRequestDescription <$> o .: "name" <*> o .: "scope" <*> o .: "visible" <*> o .: "input" <*> o .: "output" <*> o .:? "requestUrl"

instance ToJSON ApiTypeDescription where
  toJSON (ApiTypeMany a) = object [ "many" .= a ]
  toJSON (ApiTypeOptional a) = object [ "optional" .= a ]
  toJSON (ApiTypeTuple as) = toJSON as
  toJSON (ApiTypeDescription { _apiTypeName = nm, _apiTypeSource = src, _apiTypeDescriptionUrl = url}) =
    object ([ "name" .= nm
            , "source" .= src ] ++
            maybe mempty (\url' -> [ "descriptionUrl" .= show url' ]) url)
instance FromJSON ApiTypeDescription where
  parseJSON x = ApiTypeTuple <$> parseJSON x <|>
                withObject "ApiTypeDescription" (\o -> ApiTypeMany <$> o .: "many" <|>
                                                       ApiTypeOptional <$> o .: "optional" <|>
                                                       ApiTypeDescription <$> o .: "name" <*> o .: "source" <*> o .:? "descriptionUrl") x

instance ToJSON ApiCompoundTypeSchema where
  toJSON descr =
    object [ "fields" .= (descr ^. apiCompoundTypeFields)
           , "sections" .= (flip map (descr ^. apiCompoundTypeSections) $
                            \(nm, desc, fields) -> object [ "name" .= nm, "description" .= desc
                                                          , "fields" .= fields ])
           ]
instance FromJSON ApiCompoundTypeSchema where
  parseJSON = withObject "ApiCompoundTypeSchema" $ \o ->
              ApiCompoundTypeSchema <$> o .: "fields"
                                    <*> do { ls <- o .: "sections"
                                           ; forM ls $ withObject "ApiCompoundTypeSchema.fields" $ \o' ->
                                               (,,) <$> o' .: "name" <*> o' .: "description" <*> o' .: "fields" }

instance ToJSON ApiCompoundFieldSchema where
  toJSON descr =
    object [ "name"        .= (descr ^. apiCompoundFieldName)
           , "type"        .= (descr ^. apiCompoundFieldType)
           , "visible"     .= (descr ^. apiCompoundFieldVisibility)
           , "required"    .= (descr ^. apiCompoundFieldRequired)
           , "description" .= (descr ^. apiCompoundFieldDescription)
           ]
instance FromJSON ApiCompoundFieldSchema where
  parseJSON =
    withObject "ApiCompoundFieldSchema" $ \o ->
    ApiCompoundFieldSchema <$> o .: "name" <*> o .: "type" <*> o .: "visible" <*> o .: "required" <*> o .: "description"

instance ToJSON ApiTypeSource where
  toJSON ApiTypeSourceBuiltin = "builtin"
  toJSON (ApiTypeSourcePackage pkgName uri) =
    object ([ "package" .= pkgName ] ++
            maybe mempty (\uri' -> [ "descriptionUrl" .= show uri' ]) uri)
instance FromJSON ApiTypeSource where
  parseJSON "builtin" = pure ApiTypeSourceBuiltin
  parseJSON x = withObject "ApiTypeSource" (\o -> ApiTypeSourcePackage <$> o .: "package" <*> o .:? "descriptionUrl") x

instance FromJSON URI where
  parseJSON x = do Just uri <- parseURI <$> parseJSON x
                   pure uri

instance ToJSON ApiSettingsDescription where
  toJSON descr =
    object [ "fields" .= (descr ^. apiSettingsFields)
           , "sections" .= map (\(nm, s) -> object [ "name" .= nm, "fields" .= s ]) (descr ^. apiSettingsSections)
           , "errors" .= (descr ^. apiSettingsErrors) ]
instance FromJSON ApiSettingsDescription where
  parseJSON = withObject "ApiSettingsDescription" $ \v ->
              ApiSettingsDescription <$> (mapM parseSection =<< v .: "sections")
                                     <*> v .: "fields"
                                     <*> v .: "errors"
    where
      parseSection = withObject "ApiSettingsDescription.sections[]" $ \v ->
                     (,) <$> v .: "name" <*> v .: "fields"

instance ToJSON ApiSettingsSection where
  toJSON s = object [ "help"   .= (s ^. apiSettingsSectionHelp)
                    , "fields" .= (s ^. apiSettingsSectionFields) ]
instance FromJSON ApiSettingsSection where
  parseJSON = withObject "ApiSettingsSection" $ \v ->
              ApiSettingsSection <$> v .: "help" <*> v .: "fields"

instance ToJSON ApiSettingsField where
  toJSON f = object [ "name"    .= (f ^.apiSettingsFieldName)
                    , "label"   .= (f ^. apiSettingsFieldLabel)
                    , "help"    .= (f ^. apiSettingsFieldHelp)
                    , "control" .= (f ^. apiSettingsFieldControl) ]
instance FromJSON ApiSettingsField where
  parseJSON = withObject "ApiSettingsField" $ \v ->
              ApiSettingsField <$> v .: "name" <*> v .: "label" <*> v .: "help" <*> v .: "control"

instance ToJSON ApiSettingsControl where
  toJSON (ApiSettingsControlText t) =
    object [ "type" .= ("text" :: Text)
           , "value" .= t ]
  toJSON (ApiSettingsControlCalendar tm) =
    object [ "type" .= ("time" :: Text)
           , "value" .= tm ]
  toJSON (ApiSettingsControlNumber floating (minB, maxB) v) =
    object ([ "type" .= (if floating then "decimal" else "number" :: Text)
            , "value" .= v] ++
            maybe mempty (\minB' -> [ "min" .= minB' ]) minB ++
            maybe mempty (\maxB' -> [ "max" .= maxB' ]) maxB)
  toJSON (ApiSettingsControlChoices custom choices v) =
    object [ "type" .= ("choices" :: Text)
           , "value" .= v
           , "choices" .= choices
           , "customAllowed" .= custom ]
  toJSON (ApiSettingsControlLabel l) =
    object [ "type" .= ("label" :: Text)
           , "value" .= l ]
  toJSON (ApiSettingsControlCheckBox b) =
    object [ "type" .= ("checkbox" :: Text)
           , "value" .= b ]
  toJSON (ApiSettingsControlFileUpload t) =
    object ([ "type" .= ("upload" :: Text) ] ++
             maybe mempty (\(nm, uri) -> [ "fileName" .= nm
                                         , "uri" .= uri ]) t)
  toJSON (ApiSettingsControlOptional isJust t) =
    object ([ "type" .= ("optional" :: Text)
            , "isJust" .= isJust
            , "control" .= t])
  toJSON (ApiSettingsControlMulti tyName d) =
    object ["type" .= ("multi" :: Text), "dataType" .= tyName, "d" .= d]
instance FromJSON ApiSettingsControl where
  parseJSON = withObject "ApiSettingsControl" $ \v ->
              do tyNm <- v .: "type"
                 case tyNm :: String of
                   "text" -> ApiSettingsControlText <$> v .: "value"
                   "time" -> ApiSettingsControlCalendar <$> v .: "value"
                   numberNm
                     | numberNm == "decimal" || numberNm == "number" ->
                         ApiSettingsControlNumber (numberNm == "decimal") <$>
                           ((,) <$> v .:? "min" <*> v .:? "max") <*>
                           v .: "value"
                   "choices" ->
                     ApiSettingsControlChoices <$> v .: "customAllowed" <*> v .: "choices" <*> v .: "value"
                   "label" ->
                     ApiSettingsControlLabel <$> v .: "value"
                   "checkbox" ->
                     ApiSettingsControlCheckBox <$> v .: "value"
                   "upload" -> do
                     fileNm <- v .:? "fileName"
                     uri <- v .:? "uri"
                     pure (ApiSettingsControlFileUpload ((,) <$> fileNm <*> uri))
                   "multi" ->
                     ApiSettingsControlMulti <$> v .: "dataType" <*> v .: "d"
                   "optional" ->
                     ApiSettingsControlOptional <$> v .: "isJust" <*> v .: "control"
                   _ -> fail ("Unrecognized control: " ++ tyNm)

instance ToJSON ScopeContext where
  toJSON ScopeContextGlobal = "global"
  toJSON (ScopeContextUser user) = object [ "user" .= user ]
  toJSON (ScopeContextApi user api) = object [ "user" .= user, "api" .= api]
instance FromJSON ScopeContext where
  parseJSON "global" = pure ScopeContextGlobal
  parseJSON v = withObject "ScopeContext"
                (\o -> (ScopeContextApi <$> o .: "user" <*> o .: "api") <|>
                       (ScopeContextUser <$> o .: "user")) v

formatHttpTime :: UTCTime -> ByteString
formatHttpTime tm =
  fromString $ formatTime defaultTimeLocale fmt tm
  where
    fmt = "%a, %d %b %Y %H:%M:%S %Z"

mkHapiRequest :: Int -> Wai.Request -> IO HapiHttpRequest
mkHapiRequest maxBodySz req = do
  let getAtMost 0 a = pure a
      getAtMost sz a = do
        chunk <- Wai.requestBody req
        if BS.null chunk
          then pure a
          else if BS.length chunk > sz
               then pure (a <> BL.fromChunks [BS.take sz chunk])
               else getAtMost (sz - BS.length chunk) (a <> BL.fromChunks [ chunk ])

  reqBody <- BL.toStrict <$> getAtMost maxBodySz mempty

  pure $ HapiHttpRequest (TE.decodeUtf8 (Wai.requestMethod req))
                         (Wai.pathInfo req)
                         (Wai.isSecure req)
                         (map (\(nm, val) -> (TE.decodeUtf8 nm, TE.decodeUtf8 <$> val)) $
                          Wai.queryString req)
                         (TE.decodeUtf8 reqBody)
                         (map (\(nm, val) ->
                                  ( TE.decodeUtf8 (CI.foldedCase nm)
                                  , TE.decodeUtf8 val )) $
                           Wai.requestHeaders req)

fromHapiResponse :: HapiHttpResponse -> Wai.Response
fromHapiResponse resp =
  Wai.responseLBS waiStatus headers body
  where
    waiStatus = Wai.Status (resp ^. httpResponseStatus) (TE.encodeUtf8 (resp ^. httpResponsePhrase))
    headers = map (\(nm, val) -> (CI.mk (TE.encodeUtf8 nm), TE.encodeUtf8 val)) (resp ^. httpResponseHeaders)
    body = BL.fromChunks [TE.encodeUtf8 (resp ^. httpResponseBody)]

data EndpointOrContinuation
  = Endpoint Text
  | Continuation AsyncToken
  deriving Show

fromPublicEndpointLink :: Text -> Maybe EndpointOrContinuation
fromPublicEndpointLink nm
  | "endpoint-" `T.isPrefixOf` nm = Just $ Endpoint nm
  | otherwise = Continuation . AsyncToken <$> T.stripPrefix "cont-" nm
