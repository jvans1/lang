module Main where
import Parser.Parser(parse)
import SemanticAnalysis.TypeResolution
import System.Environment(getArgs)

main :: IO ()
main = do
  f <- readFile "source.txt"
  case parse f of
    Right a -> case resolve a of
                  Right a -> print a
                  Left a -> print a
    Left a -> print a
