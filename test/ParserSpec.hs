module ParserSpec where
import Parser.Parser(parse)
import Parser.Types(Expr(..), Type(..))
import Test.Hspec
run :: Spec
run = do
  let shouldSucessfullyParseTo parsed res = case head <$> parsed of
                                              Right a  -> a `shouldBe` res
                                              Left  s   -> error $ show s
  describe "parsing" $ do
    it "should return variables" $ do
      let result = parse "a"
      result `shouldSucessfullyParseTo` (Var "a")

    it "should return digits" $ do
      let result = parse "123"
      result `shouldSucessfullyParseTo` (Digit 123)

    it "parses variable assignments" $ do
      let result = parse "a = foo()"
      result `shouldSucessfullyParseTo` (Assignment (Var "a") (Call "foo" []))

    it "calls functions" $ do
      let result = parse "foo(2, 3)"
      result `shouldSucessfullyParseTo` (Call "foo" [Digit 2, Digit 3])

    it "recursively descends the structure" $ do
      let result = parse "foo(bar(baz(2)), 3)"
      let baz = (Call "baz" [Digit 2])
      let bar = (Call "bar" [baz])
      result `shouldSucessfullyParseTo` (Call "foo" [bar, Digit 3])

    it "declares functions" $ do
      let result = parse "tydef two(Integer) => Integer\nfndef boo(a)\n2\nend"
      result `shouldSucessfullyParseTo` (Function "two" [Integer] (Integer) [Var "a"] (Digit 2) )

    it "parses multiple variables separated by comma" $ do
      let result = parse "tydef two(Integer, Integer) => Integer\nfndef boo(a, b)\n2\nend"
      result `shouldSucessfullyParseTo` (Function "two" [Integer, Integer] (Integer) [Var "a", Var "b"] (Digit 2) )

    it "treats mathematcial operations like function calls" $ do
      let result = parse "3 + 4"
      result `shouldSucessfullyParseTo` (Call "+" [Digit 3, Digit 4] )

    it "allows functions to end in whitespace" $ do
      let result = parse "tydef hello() => Integer\nfndef boo()\n2  \nend"
      result `shouldSucessfullyParseTo` (Function "hello" [] (Integer) [] (Digit 2) )
