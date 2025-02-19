module App.Fossa.ArchiveUploader (
  archiveUploadSourceUnit,
  archiveNoUploadSourceUnit,
  VendoredDependency (..),
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Lift
import Control.Effect.Path (withSystemTempDir)
import Crypto.Hash
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson.Extra
import Data.ByteString.Lazy qualified as BS
import Data.Functor.Extra ((<$$>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String.Conversion
import Data.Text (Text)
import Fossa.API.Types
import Path hiding ((</>))
import Srclib.Types (Locator (..))
import System.FilePath.Posix

data VendoredDependency = VendoredDependency
  { vendoredName :: Text
  , vendoredPath :: Text
  , vendoredVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON VendoredDependency where
  parseJSON = withObject "VendoredDependency" $ \obj ->
    VendoredDependency <$> obj .: "name"
      <*> obj .: "path"
      <*> (unTextLike <$$> obj .:? "version")
      <* forbidMembers "vendored dependencies" ["type", "license", "url", "description"] obj

uploadArchives :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => ApiOpts -> [VendoredDependency] -> Path Abs Dir -> Path Abs Dir -> m [Archive]
uploadArchives apiOpts deps arcDir tmpDir = traverse (compressAndUpload apiOpts arcDir tmpDir) deps

compressAndUpload :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => ApiOpts -> Path Abs Dir -> Path Abs Dir -> VendoredDependency -> m Archive
compressAndUpload apiOpts arcDir tmpDir dependency = do
  compressedFile <- sendIO $ compressFile tmpDir arcDir (toString $ vendoredPath dependency)

  depVersion <- case vendoredVersion dependency of
    Nothing -> sendIO $ hashFile compressedFile
    Just version -> pure version

  signedURL <- Fossa.getSignedURL apiOpts depVersion (vendoredName dependency)

  _ <- Fossa.archiveUpload signedURL compressedFile

  pure $ Archive (vendoredName dependency) depVersion

-- archiveUploadSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it uploads each vendored dependency and queues a build for the dependency.
archiveUploadSourceUnit :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> ApiOpts -> [VendoredDependency] -> m [Locator]
archiveUploadSourceUnit baseDir apiOpts vendoredDeps = do
  archives <- withSystemTempDir "fossa-temp" (uploadArchives apiOpts vendoredDeps baseDir)

  -- archiveBuildUpload takes archives without Organization information. This orgID is appended when creating the build on the backend.
  -- We don't care about the response here because if the build has already been queued, we get a 401 response.
  _ <- Fossa.archiveBuildUpload apiOpts (ArchiveComponents archives)

  -- The organizationID is needed to prefix each locator name. The FOSSA API automatically prefixes the locator when queuing the build
  -- but not when reading from a source unit.
  Fossa.Organization orgId _ <- Fossa.getOrganization apiOpts

  let updateArcName :: Text -> Archive -> Archive
      updateArcName updateText arc = arc{archiveName = updateText <> "/" <> archiveName arc}
      archivesWithOrganization = updateArcName (toText $ show orgId) <$> archives

  pure $ arcToLocator <$> archivesWithOrganization

-- archiveNoUploadSourceUnit exists for when users run `fossa analyze -o` and do not upload their source units.
archiveNoUploadSourceUnit :: [VendoredDependency] -> [Locator]
archiveNoUploadSourceUnit = map (arcToLocator . forceVendoredToArchive)

forceVendoredToArchive :: VendoredDependency -> Archive
forceVendoredToArchive dep = Archive (vendoredName dep) (fromMaybe "" $ vendoredVersion dep)

arcToLocator :: Archive -> Locator
arcToLocator arc =
  Locator
    { locatorFetcher = "archive"
    , locatorProject = archiveName arc
    , locatorRevision = Just $ archiveVersion arc
    }

compressFile :: Path Abs Dir -> Path Abs Dir -> FilePath -> IO FilePath
compressFile outputDir directory fileToTar = do
  let finalFile = toString outputDir </> safeSeparators fileToTar
  entries <- Tar.pack (toString directory) [fileToTar]
  BS.writeFile finalFile $ GZip.compress $ Tar.write entries
  pure finalFile

md5 :: BS.ByteString -> Digest MD5
md5 = hashlazy

hashFile :: FilePath -> IO Text
hashFile fileToHash = do
  fileContent <- BS.readFile fileToHash
  pure . toText . show $ md5 fileContent

safeSeparators :: FilePath -> FilePath
safeSeparators = intercalate "_" . splitDirectories
