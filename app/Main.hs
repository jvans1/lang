module Main where
import Parser.Parser(parse)
import SemanticAnalysis.TypeResolution(typeChecker)
import Types
import System.Environment(getArgs)

main :: IO ()
main = do
  f <- readFile "source.txt"
  case parse f of
    Right program -> case runTypeChecker program typeChecker of
                        Right a -> print a
                        Left a -> print a
    Left failure -> print failure
