module Strategy.Fpm (discover) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipSome), findFileNamed, walk')
import Effect.ReadFS (Has, ReadFS)
import Path
import Strategy.Fortran.FpmToml (analyzeFpmToml)
import Types (DependencyResults (..), DiscoveredProject (..), GraphBreadth (Partial))

discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has ReadFS rsig run
  , Has Diagnostics rsig run
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject run]
discover dir = context "Fpm" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [FpmProject]
findProjects = walk' $ \dir _ files -> do
  let fmpSpecFile = findFileNamed "fpm.toml" files
  case (fmpSpecFile) of
    Just fpmToml -> pure ([FpmProject fpmToml dir], WalkSkipSome ["build"])
    Nothing -> pure ([], WalkContinue)

data FpmProject = FpmProject
  { fpmSpec :: Path Abs File
  , fpmSpecDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => FpmProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "fpm"
    , projectBuildTargets = mempty
    , projectDependencyResults = const $ getDeps project
    , projectPath = fpmSpecDir project
    , projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => FpmProject -> m DependencyResults
getDeps project = do
  graph <- analyzeFpmToml $ fpmSpec project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [fpmSpec project]
      }
