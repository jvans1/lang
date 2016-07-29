{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Types where
import Data.Text
import Control.Monad.Reader
import Control.Monad.Except

import Base
import Data.HashMap.Strict(HashMap)

data Type = Integer | String deriving (Show, Eq)

data Expr = 
  Function {
        fnName :: Text 
      , fnArgTypes :: [Type] 
      , retType :: Type 
      , fnArgs :: [Expr] 
      , fnbody :: NonEmpty Expr
      , _lexeme :: Lexeme
    }
  | Assignment Expr Expr Lexeme
  | Call Text [Expr] Lexeme
  | Var Text Lexeme
  | Lit Lit Lexeme deriving (Show, Eq)

data Lit = StringLit Text | Digit Integer deriving (Show, Eq)

data Lexeme = Lexeme {
    sourceLoc :: Int
  , sourceCode :: Text 
} deriving (Show, Eq)


lexeme :: Expr -> Lexeme
lexeme Function {..}        = _lexeme
lexeme (Assignment _ _ lex) = lex
lexeme (Call _ _ lex)       = lex
lexeme (Var _ lex)          = lex
lexeme (Lit _ lex)          = lex

location :: Expr -> Text
location = tshow . sourceLoc . lexeme 

source :: Expr -> Text
source = sourceCode . lexeme 

data TypeError = MisMatch | NakedExpression Text Text | UnknownFunction Text Text deriving (Show, Eq)

newtype TypeChecker a = TypeChecker {
  _runTypeChecker :: ExceptT TypeError (Reader [Expr]) a
} deriving (Functor, Applicative, Monad, MonadReader [Expr], MonadError TypeError)

runTypeChecker :: [Expr] -> TypeChecker Program -> Either TypeError Program
runTypeChecker exprs = (flip runReader exprs) . runExceptT . _runTypeChecker

data TypedFunction = TypedFunction {
    retStatement :: TypedExpr
  , typedFnName :: Text
  , tyfnArgTypes :: [Type]
  , tyRetType :: Type
  , tyFnArgs :: [TypedExpr]
  , tyFnbody :: [TypedExpr]
} deriving (Show, Eq)

type TypedExpr = (Type, Expr)

type Program = HashMap Text TypedFunction
