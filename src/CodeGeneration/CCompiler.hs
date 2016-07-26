module CodeGeneration.CCompiler where
  
import Parser.Parser(parse)
import Control.Monad((<=<))
import CodeGeneration.CodeGeneration(generate)
import SemanticAnalysis.TypeResolution(resolve)
import Store(store)

compile :: String -> String
compile program = do
  let result = (generate <=< resolve <=< store <=< parse) program
  case result of
    Right a  -> a
    Left a  -> "failed parse: " ++ show a
