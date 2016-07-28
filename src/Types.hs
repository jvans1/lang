module Types where
import Data.Text
import Base
import Data.HashMap.Strict(HashMap)

data Type = Integer | String deriving (Show, Eq)

data Function = Function {
        fnName :: Text 
      , fnArgTypes :: [Type] 
      , retType :: Type 
      , fnArgs :: [Expr] 
      , fnbody :: NonEmpty Expr
    } deriving (Show, Eq)

data Program = Program {
  functions :: HashMap Text Function
} deriving (Show, Eq)


data Expr = Assignment Expr Expr
  | Call Text [Expr]
  | Var Text
  | StringLiteral Text
  | Digit Integer deriving (Show, Eq)

--Resolve Types
--

data TypedFunction = TypedFunction {
    retStatement :: TypedExpr
  , typedFnName :: Text
  , tyfnArgTypes :: [Type]
  , tyRetType :: Type
  , tyFnArgs :: [TypedExpr]
  , tyFnbody :: [TypedExpr]
} deriving (Show, Eq)

type TypedExpr = (Type, Expr)

data TypedProgram = TypedProgram {
  typedFns ::  HashMap Text TypedFunction
} deriving (Show, Eq)
