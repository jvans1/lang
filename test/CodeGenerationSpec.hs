{-# LANGUAGE FlexibleContexts #-}
module CodeGenerationSpec where
import CodeGeneration.CGenerator(generate)
import Base
import Types
import Test.Hspec

run :: Spec
run = do
  describe "generating C code" $ do
    it "creates a function that returns an integer" $ do
      let function = TypedFunction {
          retStatement = undefined
        , typedFnName = "digit"
        , tyRetType = Integer
        , tyFnArgs = []
        , tyFnbody = [(Integer, Lit (Digit 4) undefined)]
      }


      generate [function] `shouldBe` "int digit(){ return 4; }"
      
