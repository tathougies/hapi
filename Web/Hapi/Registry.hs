module Web.Hapi.Registry where

import           Web.Hapi.Config
import           Web.Hapi.Plugin.Idl
import           Web.Hapi.Types

import           Control.Lens

import           Data.Aeson
import           Data.Data.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE

import           Network.URI

import           Turtle hiding (view, executable, f, l, FilePath)

newtype HapiPluginRegistry
  = HapiPluginRegistry { _registryApis :: M.Map ApiName HapiPluginRegistration }
  deriving (Show, Monoid)

data HapiPluginRegistration
  = HapiPluginRegistration
  { _pluginRegistrationName        :: ApiName
  , _pluginRegistrationDescription :: ApiDescription
  , _pluginExecutable              :: FilePath
  } deriving Show

data ApiLinkifier
  = ApiLinkifier
  { linkLookupDescriptionUrl :: ApiName -> ApiLookupDescription -> Maybe URI
  , linkLookupUrl            :: ApiName -> ApiLookupDescription -> Maybe URI
  , linkTypeDescriptionUrl   :: ApiName -> ApiTypeDescription   -> Maybe URI
  , linkPackageUrl           :: PackageName          -> Maybe URI
  , linkRequestUrl           :: ApiName -> ApiRequestDescription -> Maybe URI
  }

makeLenses ''HapiPluginRegistry
makeLenses ''HapiPluginRegistration

registryFromConfig :: HapiConfig -> IO HapiPluginRegistry
registryFromConfig = fmap mconcat .
                     mapM registerPlugin .
                     view hapiConfigPluginSources

registerPlugin :: HapiPluginSource -> IO HapiPluginRegistry
registerPlugin (HapiPluginSource executable) = do
  pluginName <- ApiName . T.strip <$> fetchPluginText (fromString executable) [ "pluginname" ]

  pluginDescrData <- fetchPluginText (fromString executable) [ "plugindescribe" ]
  case decode (TE.encodeUtf8 (TL.fromStrict pluginDescrData)) of
    Nothing -> fail "No description"
    Just pluginApiDescr -> do
      let registration = HapiPluginRegistration pluginName pluginApiDescr executable
      pure (HapiPluginRegistry (M.singleton pluginName registration))

fetchPluginText :: T.Text -> [ T.Text ] -> IO T.Text
fetchPluginText exec args = do
  (ec, out) <- procStrict exec args empty
  case ec of
    ExitSuccess -> pure out
    ExitFailure f -> fail ("Plugin threw error " ++ show f)

crossLinkRegistry :: ApiLinkifier -> HapiPluginRegistry -> HapiPluginRegistry
crossLinkRegistry linkifier = registryApis . each %~ crossLinkPlugin linkifier

crossLinkPlugin :: ApiLinkifier -> HapiPluginRegistration -> HapiPluginRegistration
crossLinkPlugin ApiLinkifier {..} api =
  api & pluginRegistrationDescription . biplate %~ linkifyLookup
      & pluginRegistrationDescription . biplate %~ linkifyType
      & pluginRegistrationDescription . biplate %~ linkifyTypeSource
      & pluginRegistrationDescription . biplate %~ linkifyRequest

  where
    pkg = api ^. pluginRegistrationName

    linkifyLookup l =
      l & apiLookupDescriptionUrl .~ linkLookupDescriptionUrl pkg l
        & apiLookupUrl .~ linkLookupUrl pkg l
    linkifyRequest r=
      r & apiRequestUrl .~ linkRequestUrl pkg r
    linkifyType t = t & apiTypeDescriptionUrl .~ linkTypeDescriptionUrl pkg t

    linkifyTypeSource t = t & apiTypeSourcePackageUrl .~ (linkPackageUrl =<< t ^? apiTypeSourcePackage)
