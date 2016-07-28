import Base
import qualified ParserSpec
import qualified TypeResolutionSpec
import qualified TypeCheckingSpec

import Test.Hspec
main :: IO ()
main = hspec $ do
  ParserSpec.run
  TypeResolutionSpec.run
  TypeCheckingSpec.run
