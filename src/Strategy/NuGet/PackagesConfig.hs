{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.PackagesConfig (
  discover,
  findProjects,
  getDeps,
  mkProject,
  buildGraph,
  PackagesConfig (..),
  NuGetDependency (..),
) where

import Control.Effect.Diagnostics
import Data.Foldable (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import Graphing qualified
import Parse.XML
import Path
import Types

-- discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
-- discover dir = context "PackagesConfig" $ do
--   projects <- context "Finding Projects" $ findProjects dir
--   pure (map mkProject projects)

discover = undefined
mkProject = undefined

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [PackagesConfigProject]
findProjects = walk' $ \_ _ files -> do
  case find (\f -> fileName f == "packages.config") files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([PackagesConfigProject file], WalkContinue)

newtype PackagesConfigProject = PackagesConfigProject
  { packagesConfigFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

-- mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => PackagesConfigProject -> DiscoveredProject n
-- mkProject project =
--   DiscoveredProject
--     { projectType = "packagesconfig"
--     , projectBuildTargets = mempty
--     , projectDependencyResults = const $ getDeps project
--     , projectPath = parent $ packagesConfigFile project
--     , projectLicenses = pure []
--     }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PackagesConfigProject -> m DependencyResults
getDeps = context "PackagesConfig" . context "Static analysis" . analyze' . packagesConfigFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  config <- readContentsXML @PackagesConfig file
  graph <- context "Building dependency graph" $ pure (buildGraph config)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [file]
      }

instance FromXML PackagesConfig where
  parseElement el = PackagesConfig <$> children "package" el

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency <$> attr "id" el
      <*> attr "version" el

newtype PackagesConfig = PackagesConfig
  { deps :: [NuGetDependency]
  }
  deriving (Eq, Ord, Show)

data NuGetDependency = NuGetDependency
  { depID :: Text
  , depVersion :: Text
  }
  deriving (Eq, Ord, Show)

buildGraph :: PackagesConfig -> Graphing Dependency
buildGraph = Graphing.fromList . map toDependency . deps
  where
    toDependency NuGetDependency{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = depID
        , dependencyVersion = Just (CEq depVersion)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
