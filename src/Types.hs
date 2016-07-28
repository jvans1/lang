module Types where
import Data.Text
import Base
import Data.HashMap.Strict(HashMap)

data Type = Integer deriving (Show, Eq)

data Function = Function {
        fnName :: Text 
      , fnArgTypes :: [Type] 
      , retType :: Type 
      , fnArgs :: [Expr] 
      , fnbody :: Expr
    } deriving (Show, Eq)

data Program = Program {
  functions :: HashMap Text Function
} deriving (Show, Eq)

data Expr = Assignment Expr Expr
  | Call Text [Expr]
  | Var Text
  | Digit Integer deriving (Show, Eq)
