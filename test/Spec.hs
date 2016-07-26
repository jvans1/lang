import qualified ParserSpec
import qualified CodeGenerationSpec
import qualified TypeResolutionSpec
import qualified CCompilerSpec

import Test.Hspec
main :: IO ()
main = hspec $ do
  ParserSpec.run
  CodeGenerationSpec.run
  TypeResolutionSpec.run
  CCompilerSpec.run
