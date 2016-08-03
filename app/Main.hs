{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
module Main where
import Parser.Parser(parse)
import Base
import SemanticAnalysis.TypeResolution(typeChecker)
import Types
import System.Environment(getArgs)

main :: IO ()
main = do
  f <- readFile "source.txt"
  case parse f of
    Right program -> case runTypeChecker program typeChecker of
                        Right a -> print a
                        Left (NakedExpression lineno expr)-> putStrLn $ lineno ++ "Naked Expression at top level: " ++ expr
                        Left (UnknownFunction x lineno)-> putStrLn $ "Unknown function invocation on line: " ++ lineno ++ "; '" ++ x ++ "()' is not defined."

    Left failure -> print failure
