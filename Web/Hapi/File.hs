module Web.Hapi.File where

import           Web.Hapi.Types

import qualified Data.ByteString as BS
import           Data.UUID (UUID)

import           Network.Wai.Parse

data HapiFileBackend
  = HapiFileBackend
  { fileBackendSave :: BackEnd (Maybe (UUID, HapiObject))
  , fileBackendRead :: HapiObject -> IO (IO BS.ByteString)
  }
