module SemanticAnalysis.Types where
import Data.Text(Text)
import Parser.Types
import Data.HashMap.Strict(HashMap)

data Variable = Variable Type Text

data Instruction = FnCall Text [Variable]

data Function = Function {
  name :: Text
  , varArgs :: [Variable]
  , instructions :: [Instruction]
}

data Program = Program {
  functions :: HashMap Text Function
}
