{-# LANGUAGE RecordWildCards #-}

module Strategy.Node.PackageJson (
  buildGraph,
  analyze',
  PackageJson (..),
) where

import Control.Effect.Diagnostics
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import DepTypes (
  DepEnvironment (..),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CCompatible),
  insertEnvironment,
 )
import Effect.Grapher (
  LabeledGrapher,
  direct,
  label,
  withLabeling,
 )
import Effect.ReadFS
import Graphing (Graphing)
import Path

data PackageJson = PackageJson
  { packageDeps :: Map Text Text
  , packageDevDeps :: Map Text Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON PackageJson where
  parseJSON = withObject "PackageJson" $ \obj ->
    PackageJson <$> obj .:? "dependencies" .!= Map.empty
      <*> obj .:? "devDependencies" .!= Map.empty

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  packageJson <- readContentsJson @PackageJson file
  context "Building dependency graph" $ pure (buildGraph packageJson)

-- TODO: decode version constraints
data NodePackage = NodePackage
  { pkgName :: Text
  , pkgConstraint :: Text
  }
  deriving (Eq, Ord, Show)

type NodeGrapher = LabeledGrapher NodePackage NodePackageLabel

newtype NodePackageLabel = NodePackageEnv DepEnvironment
  deriving (Eq, Ord, Show)

buildGraph :: PackageJson -> Graphing Dependency
buildGraph PackageJson{..} = run . withLabeling toDependency $ do
  _ <- Map.traverseWithKey (addDep EnvProduction) packageDeps
  _ <- Map.traverseWithKey (addDep EnvDevelopment) packageDevDeps
  pure ()
  where
    addDep :: Has NodeGrapher sig m => DepEnvironment -> Text -> Text -> m ()
    addDep env name constraint = do
      let pkg = NodePackage name constraint
      direct pkg
      label pkg (NodePackageEnv env)

    toDependency :: NodePackage -> Set NodePackageLabel -> Dependency
    toDependency dep = foldr addLabel (start dep)

    addLabel :: NodePackageLabel -> Dependency -> Dependency
    addLabel (NodePackageEnv env) = insertEnvironment env

    start :: NodePackage -> Dependency
    start NodePackage{..} =
      Dependency
        { dependencyType = NodeJSType
        , dependencyName = pkgName
        , dependencyVersion = Just (CCompatible pkgConstraint)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
