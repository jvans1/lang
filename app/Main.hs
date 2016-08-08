{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
module Main where
import Parser.Parser(parse)
import Base
import SemanticAnalysis.TypeChecking(typeCheck)
import Types
import System.Environment(getArgs)

main :: IO ()
main = do
  f <- readFile "source.txt"
  case parse f of
    Right program -> case typeCheck program of
                        Right a -> print a
                        Left xs -> mapM_ writeErr xs

    Left failure -> print failure


writeErr :: TypeError -> IO ()
writeErr (NakedExpression expr) = putStrLn $ location expr ++ " naked expression at top level: " ++ tshow expr
writeErr (UnknownFunction x lineno)    = putStrLn $ "Unknown function invocation on line: " ++ lineno ++ "; '" ++ x ++ "()' is not defined."
writeErr (MisMatch type1 texpr)        = putStrLn $ "Type mismatch at line: " ++ (location $ expr texpr) ++  "\nexpected " ++ tshow type1 ++ " but got " ++ tshow (exprType texpr)
writeErr (DuplicateDeclaration name expr)    = putStrLn $ "Duplicate declaration of " ++ name ++ " on " ++ location expr
writeErr (UndefinedVariable name expr)    = putStrLn $ "Undefined variable: " ++ name ++ " on " ++ location expr
