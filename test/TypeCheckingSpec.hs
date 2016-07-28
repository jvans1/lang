module TypeCheckingSpec where
import Test.Hspec
import Base
import Parser.Parser(parse)
import Types
import SemanticAnalysis.TypeChecking

run :: Spec 
run = do
  describe "Type Checking" $ do
    it "does not let you return the wrong different type " $ do 
      f <- liftIO $ readFile "./test/TypeCheckingExamples.txt"
      case parse f of
        Right a -> case check a of
                     Right _ -> True `shouldBe` True
                     {- Left _ -> error "failure" -}
        Left b -> error $ "Failed Parse attempt" ++ show b
      

