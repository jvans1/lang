import qualified ParserSpec
import qualified TypeResolutionSpec

import Test.Hspec
main :: IO ()
main = hspec $ do
  ParserSpec.run
  TypeResolutionSpec.run
