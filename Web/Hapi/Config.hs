module Web.Hapi.Config where

import           Control.Lens

import           Data.Text ( Text )
import           Data.Yaml.Include ( decodeFile )
import           Data.Aeson ( FromJSON(..), genericParseJSON, defaultOptions )
import           Data.Aeson.Types ( Options(..), camelTo2 )

import qualified Data.Pool as P
import           Data.String
import           Data.Text.Lens
import qualified Database.PostgreSQL.Simple as Db

import           GHC.Generics

newtype HapiPluginSource = HapiPluginSource FilePath
  deriving (Show, Eq, FromJSON)

data HapiConfig
  = HapiConfig
  { _hapiConfigPluginSources  :: [ HapiPluginSource ]
  , _hapiConfigPluginDatabase :: Text
  , _hapiConfigMemcache :: String
  , _hapiConfigJwtSecret :: Text
  } deriving (Show, Generic)
makeLenses ''HapiConfig

dropPrefix :: [Char] -> [Char] -> [Char]
dropPrefix [] xs = xs
dropPrefix (p:ps) (x:xs)
  | p == x = dropPrefix ps xs
dropPrefix _ xs = xs

instance FromJSON HapiConfig where
  parseJSON = genericParseJSON
                defaultOptions { fieldLabelModifier =
                                   camelTo2 '-' .
                                   dropPrefix "_hapiConfig" }

readHapiConfig :: FilePath -> IO (Maybe HapiConfig)
readHapiConfig = decodeFile

newDbPoolFromConfig :: HapiConfig -> IO (P.Pool Db.Connection)
newDbPoolFromConfig config =
  P.createPool (Db.connectPostgreSQL (fromString (config ^. hapiConfigPluginDatabase . unpacked)))
               Db.close
               1 2 1000
