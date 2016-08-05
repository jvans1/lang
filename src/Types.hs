{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Types where
import Control.Monad.State.Lazy(MonadState, State, evalState)
import Data.Text
import Control.Monad.Reader
import Control.Monad.Except

import Base
import Data.HashMap.Strict(HashMap)
import Control.Monad.Writer.Lazy(MonadWriter, WriterT, runWriterT)

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

data TypeError = DuplicateDeclaration Text Expr |  MisMatch Type TypedExpr | NakedExpression Expr | InvalidEntry | UnknownFunction Text Text deriving (Show, Eq)

exprType :: TypedExpr -> Type
exprType = fst

expr :: TypedExpr -> Expr
expr = snd

type Scope = HashMap Text (Type, Expr)

newtype TypeAssignment a = TypeAssignment {
  runTypeAssignment :: ExceptT TypeError (State Scope) a
} deriving (Functor, Applicative, Monad, MonadState Scope, MonadError TypeError)

assignTypes :: HashMap Text (Type, Expr) -> TypeAssignment Program -> Either TypeError Program
assignTypes exprs typeassignment = evalState (runExceptT $ runTypeAssignment typeassignment) exprs


newtype TypeChecker a = TypeChecker {
   _runTypeChecker :: WriterT [TypeError] (Reader Program) a
} deriving (Functor, Applicative, Monad, MonadWriter [TypeError], MonadReader Program)

runTypeChecker :: Program -> TypeChecker Program -> (Program, [TypeError])
runTypeChecker prgm tycheck = flip runReader prgm $ runWriterT (_runTypeChecker tycheck)

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
