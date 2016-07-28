module CodeGenerationSpec where

import Data.HashMap.Strict(singleton)
import qualified CodeGeneration.Types as CT
import SemanticAnalysis.Types
import Types
import CodeGeneration.CodeGeneration
import Test.Hspec
run :: Spec
run = do
  describe "code generation" $ do
    it "transforms an AST into a data structure that represents a C function" $ do
      let fn  = Function {
        name = "add_two"
        , body = [
          FnCall Add (Argument "a" Integer) (Argument "b" Integer)
        ]
      }

      let ast = Program { functions = singleton "add_two" fn }
      let instruction = FnCall Add (Argument "a" Integer) (Argument "b" Integer)
      generate' ast `shouldBe`  [CT.CFunction (Just instruction) [("a", Integer), ("b", Integer)] "add_two"]
