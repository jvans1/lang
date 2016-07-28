{-# LANGUAGE DeriveFunctor #-}
module SemanticAnalysis.Types where
import Data.Text(Text)
import Types
import Data.HashMap.Strict(HashMap)

data Function = Function {
  name :: Text
  , body :: [Instruction]
} deriving (Show, Eq)

data BinaryOp = Add deriving (Show, Eq)
data Instruction = FnCall BinaryOp Instruction Instruction | Argument Text Type deriving (Show, Eq)

data Functions a = Program {
  functions :: HashMap Text a
} deriving (Show, Eq, Functor)

type Program = Functions Function
