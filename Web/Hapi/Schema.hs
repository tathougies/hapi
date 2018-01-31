module Web.Hapi.Schema where

import Web.Hapi.Types (HapiObject, HapiLookupKey)

import Control.Lens

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import Database.Beam.Postgres.PgCrypto

import Data.UUID (UUID)
import Data.Time (LocalTime)
import Data.Text (Text)

-- * Users

data UserT f
  = User
  { _userId      :: C f UUID
  , _userHomepage :: C f Text
  , _userCreated :: C f LocalTime
  } deriving Generic
instance Beamable UserT

instance Table UserT where
  data PrimaryKey UserT f = UserKey { userKeyId :: C f UUID }
    deriving Generic
  primaryKey = UserKey <$> _userId
instance Beamable (PrimaryKey UserT)

makeLenses ''UserT

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

type UserKey = PrimaryKey UserT Identity
deriving instance Show UserKey
deriving instance Eq UserKey

deriving instance Show (PrimaryKey UserT (Nullable Identity))
deriving instance Eq (PrimaryKey UserT (Nullable Identity))

-- * Apis

data ApiRegistrationT f
  = ApiRegistration
  { _apiId      :: C f UUID
  , _apiName    :: C f Text
  , _apiCreated :: C f LocalTime
  , _apiUser    :: PrimaryKey UserT f
  , _apiData    :: C f (PgJSONB HapiObject)
  } deriving Generic
instance Beamable ApiRegistrationT

makeLenses ''ApiRegistrationT

instance Table ApiRegistrationT where
  data PrimaryKey ApiRegistrationT f
    = ApiRegistrationKey (C f UUID)
    deriving Generic

  primaryKey = ApiRegistrationKey <$> _apiId

instance Beamable (PrimaryKey ApiRegistrationT)

type ApiRegistration = ApiRegistrationT Identity
deriving instance Show ApiRegistration
deriving instance Eq ApiRegistration

type ApiRegistrationKey = PrimaryKey ApiRegistrationT Identity
deriving instance Show ApiRegistrationKey
deriving instance Eq ApiRegistrationKey

deriving instance Show (PrimaryKey ApiRegistrationT (Nullable Identity))
deriving instance Eq (PrimaryKey ApiRegistrationT (Nullable Identity))

-- * Publicly exposed endpoints

data PublicEndpointT f
  = PublicEndpoint
  { _endpointPath    :: C f Text
  , _endpointUser    :: PrimaryKey UserT (Nullable f)
  , _endpointApi     :: PrimaryKey ApiRegistrationT (Nullable f)
  , _endpointPlugin :: C f Text
  , _endpointRequest :: C f Text
  } deriving Generic
instance Beamable PublicEndpointT

makeLenses ''PublicEndpointT

instance Table PublicEndpointT where
  data PrimaryKey PublicEndpointT f
    = PublicEndpointKey (C f Text)
    deriving Generic
  primaryKey = PublicEndpointKey <$> _endpointPath
instance Beamable (PrimaryKey PublicEndpointT)

type PublicEndpoint = PublicEndpointT Identity
deriving instance Show PublicEndpoint
deriving instance Eq PublicEndpoint

type PublicEndpointKey = PrimaryKey PublicEndpointT Identity
deriving instance Show PublicEndpointKey
deriving instance Eq PublicEndpointKey

-- * Credentials and automatic values

data ConfigurationT f
  = Configuration
  { _configurationUser  :: PrimaryKey UserT (Nullable f)
  , _configurationApi   :: PrimaryKey ApiRegistrationT (Nullable f)
  , _configurationKey   :: C f Text
  , _configurationType  :: C f Text
  , _configurationValue :: C f (PgJSONB HapiObject)
  } deriving Generic

instance Beamable ConfigurationT

makeLenses ''ConfigurationT

instance Table ConfigurationT where
  data PrimaryKey ConfigurationT f
    = ConfigurationKey (PrimaryKey UserT (Nullable f)) (PrimaryKey ApiRegistrationT (Nullable f))
                       (C f Text)
      deriving Generic
  primaryKey = ConfigurationKey <$> _configurationUser <*> _configurationApi <*> _configurationKey
instance Beamable (PrimaryKey ConfigurationT)

type Configuration = ConfigurationT Identity
deriving instance Show Configuration
deriving instance Eq Configuration

type ConfigurationKey = PrimaryKey ConfigurationT Identity
deriving instance Show ConfigurationKey
deriving instance Eq ConfigurationKey

-- * File Upload

data FileUploadT f
  = FileUpload
  { _fileUploadId       :: C f UUID
  , _fileUploadName     :: C f Text
  , _fileUploadMimeType :: C f Text
  , _fileUploadData     :: C f (PgJSONB HapiObject)
  , _fileUploadCreatedAt :: C f LocalTime
  , _fileUploadUsed     :: C f Int
  , _fileUploadSecret   :: C f Text
  } deriving Generic
instance Beamable FileUploadT

makeLenses ''FileUploadT

instance Table FileUploadT where
  data PrimaryKey FileUploadT f
    = FileUploadKey (C f UUID)
    deriving Generic
  primaryKey = FileUploadKey <$> _fileUploadId
instance Beamable (PrimaryKey FileUploadT)

type FileUpload = FileUploadT Identity
deriving instance Show FileUpload
deriving instance Eq FileUpload

type FileUploadKey = PrimaryKey FileUploadT Identity
deriving instance Show FileUploadKey
deriving instance Eq FileUploadKey

-- * Lookup

data LookupEntryT f
  = LookupEntry
  { _lookupEntryFullKey :: C f (PgJSONB HapiLookupKey) -- Base64 encoded version of key
  , _lookupEntryUser    :: PrimaryKey UserT (Nullable f)
  , _lookupEntryApi     :: PrimaryKey ApiRegistrationT (Nullable f)
  , _lookupEntryValue   :: C f (PgJSONB HapiObject)
  } deriving Generic
instance Beamable LookupEntryT

makeLenses ''LookupEntryT

instance Table LookupEntryT where
  data PrimaryKey LookupEntryT f =
    LookupEntryKey (C f (PgJSONB HapiLookupKey))
    deriving Generic
  primaryKey = LookupEntryKey <$> _lookupEntryFullKey
instance Beamable (PrimaryKey LookupEntryT)

type LookupEntry = LookupEntryT Identity
deriving instance Show LookupEntry
deriving instance Eq LookupEntry

type LookupEntryKey = PrimaryKey LookupEntryT Identity
deriving instance Show LookupEntryKey
deriving instance Eq LookupEntryKey

-- * Database

data Db entity
  = Db
  { _dbUsers          :: entity (TableEntity UserT)
  , _dbApis           :: entity (TableEntity ApiRegistrationT)
  , _dbUploads        :: entity (TableEntity FileUploadT)
  , _dbConfigurations :: entity (TableEntity ConfigurationT)
  , _dbPublicEndpoints :: entity (TableEntity PublicEndpointT)
  , _dbLookups        :: entity (TableEntity LookupEntryT)
  , _dbCryptoModule   :: entity (PgExtensionEntity PgCrypto)
  } deriving Generic
instance Database Postgres Db

makeLenses ''Db

migration :: Migration PgCommandSyntax (CheckedDatabaseSettings Postgres Db)
migration = do
  -- Make sure the crypto module is here
  cryptoModuleEntity <- pgCreateExtension

  let cryptoModule = getPgExtension cryptoModuleEntity

  users <-
    createTable "user" $
    User (field "id" uuid notNull (defaultTo_ (pgCryptoGenRandomUUID cryptoModule)))
         (field "homepage" text notNull (defaultTo_ ""))
         (field "created" timestamp notNull (defaultTo_ currentTimestamp_))
  apiRegistrations <-
    createTable "api_registration" $
    ApiRegistration (field "id" uuid notNull (defaultTo_ (pgCryptoGenRandomUUID cryptoModule)))
                    (field "name" text notNull)
                    (field "created" timestamp notNull (defaultTo_ currentTimestamp_))
                    (UserKey (field "user" uuid notNull))
                    (field "data" jsonb notNull)
  uploads <-
    createTable "file_uploads" $
    FileUpload (field "id" uuid notNull (defaultTo_ (pgCryptoGenRandomUUID cryptoModule)))
               (field "name" text notNull)
               (field "mime_type" text notNull)
               (field "data" jsonb notNull)
               (field "created" timestamp notNull (defaultTo_ currentTimestamp_))
               (field "refcnt" int notNull)
               (field "secret" text notNull)
  configurations <-
    createTable "configuration" $
    Configuration (UserKey (field "user" (maybeType uuid))) (ApiRegistrationKey (field "api" (maybeType uuid)))
                  (field "key" text notNull)
                  (field "type" text notNull)
                  (field "value" jsonb notNull)
  publicEndpoints <-
    createTable "endpoints" $
    PublicEndpoint (field "path" text notNull)
                   (UserKey (field "user" (maybeType uuid)))
                   (ApiRegistrationKey (field "api" (maybeType uuid)))
                   (field "plugin" text notNull)
                   (field "request" text notNull)
  lookups <-
    createTable "lookups" $
    LookupEntry (field "fullKey" jsonb notNull)
                (UserKey (field "user" (maybeType uuid)))
                (ApiRegistrationKey (field "api" (maybeType uuid)))
                (field "value" jsonb notNull)
  -- TODO create index
  pure (Db users apiRegistrations uploads configurations publicEndpoints lookups cryptoModuleEntity)

hapiDb :: DatabaseSettings Postgres Db
hapiDb = unCheckDatabase (runMigrationSilenced migration)
