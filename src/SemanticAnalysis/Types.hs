{-# LANGUAGE DeriveFunctor #-}
module SemanticAnalysis.Types where
import Data.Text(Text)
import Parser.Types
import Data.HashMap.Strict(HashMap)

data Variable = Variable Type Text Instruction

data Argument = Argument Type Text

data Instruction = FnCall Text [Variable] | DataType Type

data Function = Function {
  name :: Text
  , variables :: HashMap Text Variable
  , arguments :: [Argument]
  , instructions :: [Instruction]
}

data Functions a = Program {
  functions :: HashMap Text a
} deriving (Show, Eq, Functor)

type Program = Functions Function
