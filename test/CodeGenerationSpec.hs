{-# LANGUAGE OverloadedStrings #-}
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
          program = insert ("digit" :: Text) function empty
      generate program `shouldBe` "int digit(){ return 4; }"
      
    it "accepts an arbitrary number of arguments" $ do
      let function = TypedFunction {
          retStatement = undefined
        , typedFnName = "digit"
        , tyRetType = Integer
        , tyFnArgs = [(Integer, Var "a" undefined), (Integer, Var "b" undefined), (Integer, Var "c" undefined), (Integer, Var "d" undefined)]
        , tyFnbody = [(Integer, Var "a" undefined)]
      }
          program = insert ("digit" :: Text) function empty
      generate program `shouldBe` "int digit(int a, int b, int c, int d){ return a; }"
      
