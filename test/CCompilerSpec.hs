module CCompilerSpec where
import CodeGeneration.CCompiler
import Test.Hspec

run :: Spec
run = do
  describe "Compiling to C" $ do
    it "translates functions" $ do
      let code = "tydef foo() => Integer\nfndef foo()\n 3\nend"
      let result = "int foo()\n{return 3;\n}"
      compile code `shouldBe` result

    it "translates functions with arguments" $ do
      let code = "tydef foo(Integer, Integer) => Integer\nfndef foo(a, b)\n  a + b  \nend"
      let result = "int foo(int a, int b)\n{return a + b;\n}"
      compile code `shouldBe` result
