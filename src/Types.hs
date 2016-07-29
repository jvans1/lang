{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    }
  | Assignment Expr Expr
  | Call Text [Expr]
  | Var Text
  | StringLiteral Text
  | Digit Integer deriving (Show, Eq)

--Resolve Types
--

data TypeError = MisMatch deriving (Show, Eq)

newtype TypeChecker a = TypeChecker {
  runTypeChecker :: ExceptT TypeError (Reader [Expr]) a
} deriving (Functor, Applicative, Monad, MonadReader [Expr], MonadError TypeError)

typeCheck :: [Expr] -> TypeChecker TypedProgram -> Either TypeError TypedProgram
typeCheck exprs = (flip runReader exprs) . runExceptT . runTypeChecker


data TypedFunction = TypedFunction {
    retStatement :: TypedExpr
  , typedFnName :: Text
  , tyfnArgTypes :: [Type]
  , tyRetType :: Type
  , tyFnArgs :: [TypedExpr]
  , tyFnbody :: [TypedExpr]
} deriving (Show, Eq)

type TypedExpr = (Type, Expr)

type TypedProgram = HashMap Text TypedFunction
