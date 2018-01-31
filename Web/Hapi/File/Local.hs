module Web.Hapi.File.Local
  (localFileBackend) where

import           Web.Hapi.File
import           Web.Hapi.Types

import           Control.Lens

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import           Data.String
import           Data.Text.Lens
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import           Network.Wai.Parse

import           System.Directory
import           System.FilePath
import           System.IO

localUploadBackend :: FilePath -> BS.ByteString -> BackEnd (Maybe (UUID.UUID, HapiObject))
localUploadBackend dir expNm nm _ getChunk
  | expNm /= nm = pure Nothing
  | otherwise = do
      createDirectoryIfMissing True dir

      let newId = do
            uuid <- UUID.nextRandom
            let localFileNmAbs = dir </> localFileNm
                localFileNm = uuid ^. re fileIdUuid . _FileId . unpacked

            alreadyExists <- doesFileExist localFileNmAbs
            if alreadyExists then newId else pure (uuid, localFileNmAbs)

      (newFileId, newFileNm) <- newId
      withBinaryFile newFileNm WriteMode $ \hdl ->
        let copyAll = do
              nextChunk <- getChunk
              if BS.null nextChunk
                then pure ()
                else do
                  BS.hPut hdl nextChunk
                  copyAll
        in copyAll

      pure (Just ( newFileId
                 , HapiObject (HM.fromList [ ("filePath", HapiValueText (fromString newFileNm)) ]) ))

localFileBackend :: FilePath -> HapiFileBackend
localFileBackend dir =
  HapiFileBackend (localUploadBackend dir "file")
                  (\(HapiObject obj) ->
                      case obj ^? at "filePath" . _Just . _HapiValueText . unpacked of
                        Nothing -> fail "Invalid local file data"
                        Just fileName -> do
                          hdl <- openFile fileName ReadMode
                          pure $ do
                            isEof <- hIsEOF hdl
                            if isEof
                              then pure mempty
                              else BS.hGet hdl 4096)
