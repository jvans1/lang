{-# LANGUAGE FlexibleContexts #-}
module ParserSpec where
import Parser.Parser(parse)
import Text.Parsec.Error(errorMessages, Message(..))
import Base
import Types
import Test.Hspec

run :: Spec
run = do
  describe "parsing" $ do
    it "throws an error when a top level declaration is not a function" $ do
      let result = parse "1 + 2"
      case result of
        Right a -> error $  "should not successfully parse to" ++ show a
        Left _ -> True `shouldBe` True

    it "should return variables" $ do
      let result = parse "tydef main() => Integer\nfndef main()\na\nend"
      case result of
          Right ((Function _ _ _ _ (Var "a" _:|_) _):[]) -> True `shouldBe` True
          Left a -> True `shouldBe` False

    {- it "should return digits" $ do -}
      {- let result = parse "tydef main() => Integer\nfndef main()\n123\nend" -}
      {- result `shouldSucessfullyParseTo` (Function "main" [] Integer [] (Digit 123)) -}

    {- it "allows string literals" $ do -}
      {- let result = parse "tydef main() => String\nfndef main()\n\"hello world\"\nend" -}
      {- result `shouldSucessfullyParseTo` (Function "main" [] String [] (StringLit "hello world")) -}

    {- it "parses variable assignments" $ do -}
      {- let result = parse "tydef main() => Integer\nfndef main()\n a = foo()\nend" -}
      {- result `shouldSucessfullyParseTo` (Function "main" [] Integer [] (Assignment (Var "a") (Call "foo" []))) -}

    {- it "calls functions" $ do -}
      {- let result = parse "tydef main() => Integer\nfndef main()\n foo(2, 3)\nend" -}
      {- result `shouldSucessfullyParseTo` (Function "main" [] Integer [] (Call "foo" [Digit 2, Digit 3])) -}

    {- it "recursively descends the structure" $ do -}
      {- let result = parse "tydef main() => Integer\nfndef main()\n foo(bar(baz(2)), 3)\nend" -}
      {- let baz = (Call "baz" [Digit 2]) -}
      {- let bar = (Call "bar" [baz]) -}
      {- result `shouldSucessfullyParseTo` (Function "main" [] Integer [] (Call "foo" [bar, Digit 3])) -}

    {- it "declares functions with arguments" $ do -}
      {- let result = parse "tydef main(Integer) => Integer\nfndef main(a)\n2\nend" -}
      {- result `shouldSucessfullyParseTo` (Function "main" [Integer] (Integer) [Var "a"] (Digit 2) ) -}

    {- it "parses multiple variables separated by comma" $ do -}
      {- let result = parse "tydef main(Integer, Integer) => Integer\nfndef boo(a, b)\n2\nend" -}
      {- result `shouldSucessfullyParseTo` (Function "main" [Integer, Integer] (Integer) [Var "a", Var "b"] (Digit 2) ) -}

    {- it "treats mathematcial operations like function calls" $ do -}
      {- let result = parse "tydef main() => Integer\nfndef main()\n 3 + 4 \nend" -}
      {- result `shouldSucessfullyParseTo` (Function "main" [] (Integer) [] (Call "+" [Digit 3, Digit 4] ) ) -}

    {- it "allows functions to end in whitespace" $ do -}
      {- let result = parse "tydef main() => Integer\nfndef main()\n2  \nend" -}
      {- result `shouldSucessfullyParseTo` (Function "main" [] (Integer) [] (Digit 2)) -}
