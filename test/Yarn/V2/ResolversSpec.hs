module Yarn.V2.ResolversSpec (
  spec,
) where

import Data.Foldable (for_)
import Data.String.Conversion (toString)
import Data.Text
import Strategy.Yarn.V2.Lockfile
import Strategy.Yarn.V2.Resolvers
import Test.Hspec

spec :: Spec
spec = do
  testResolver
    workspaceResolver
    [ (Locator Nothing "unused" "workspace:.", WorkspacePackage ".")
    , (Locator Nothing "unused" "workspace:bar", WorkspacePackage "bar")
    , (Locator Nothing "unused" "workspace:../baz", WorkspacePackage "../baz")
    ]

  testResolver
    npmResolver
    [ -- without a scope
      (Locator Nothing "packagename" "npm:1.0.0", NpmPackage Nothing "packagename" "1.0.0")
    , -- with a scope
      (Locator (Just "withscope") "packagename" "npm:1.0.0", NpmPackage (Just "withscope") "packagename" "1.0.0")
    ]

  testResolver
    gitResolver
    [ (Locator Nothing "unused" "https://example.com/foo.git#commit=abcdef", GitPackage "https://example.com/foo.git" "abcdef")
    , -- a case where there are several keys after #
      (Locator Nothing "unused" "https://example.com/foo.git#branch=something&commit=abcdef&otherkey=somethingelse", GitPackage "https://example.com/foo.git" "abcdef")
    ]

  testResolver
    tarResolver
    [ -- https url, .tar.gz
      (Locator Nothing "unused" "https://link.to/tarball.tar.gz", TarPackage "https://link.to/tarball.tar.gz")
    , -- http url, .tgz
      (Locator Nothing "unused" "http://link.to/tarball.tar.gz", TarPackage "http://link.to/tarball.tar.gz")
    , -- https url, .tgz
      (Locator Nothing "unused" "https://link.to/tarball.tgz", TarPackage "https://link.to/tarball.tgz")
    , -- awkward input
      (Locator Nothing "unused" "https://link.to/tarball..tgz?foo=bar", TarPackage "https://link.to/tarball..tgz?foo=bar")
    ]

  testUnsupportedResolver fileResolver "file:" FilePackage
  testUnsupportedResolver linkResolver "link:" LinkPackage
  testUnsupportedResolver portalResolver "portal:" PortalPackage
  testUnsupportedResolver execResolver "exec:" ExecPackage
  testUnsupportedResolver patchResolver "patch:" PatchPackage

testResolver ::
  Resolver ->
  -- | A list of (locator, expected package resolution) pairs
  [(Locator, Package)] ->
  Spec
testResolver resolver supported =
  describe (toString (resolverName resolver)) $ do
    it "Should work for supported locators" $ do
      for_ supported $ \(locator, result) -> do
        resolverSupportsLocator resolver locator `shouldBe` True
        resolverLocatorToPackage resolver locator `shouldBe` Right result

testUnsupportedResolver ::
  Resolver ->
  -- | Protocol prefix
  Text ->
  -- | Constructor for packages
  (Text -> Package) ->
  Spec
testUnsupportedResolver resolver protocol constructor =
  testResolver
    resolver
    [(Locator Nothing "unused" (protocol <> "somepackage"), constructor "somepackage")]
