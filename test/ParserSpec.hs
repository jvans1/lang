module ParserSpec where
import Parser(parse, Expr(..), Type(..), Op(..))
import Test.Hspec
run :: Spec
run =
  describe "parsing" $ do
    {- it "should return functions" $ do -}
      {- let result = parse "foo Integer => Integer\n a = a" -}
      {- result `shouldBe` Function "foo" [Integer] Integer [Var "a"] (Var "a") -}

    it "creates a node for addition" $ do
      let result = parse "1 + 2"
      result `shouldBe` BinaryOp Add (Digit 1) (Digit 2)

    {- it "maintaines operator precedence with multiplication" $ do -}
      {- let result = parse "1 + 2 * 3" -}
      {- result `shouldBe` BinaryOp Add (Digit 1) (BinaryOp Mult (Digit 2) (Digit 3)) -}
