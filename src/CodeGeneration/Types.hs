module CodeGeneration.Types where
import Types
import SemanticAnalysis.Types
import Data.Text


data CFunction = CFunction {
  retVal ::  Maybe Instruction
  , arguments :: [(Text, Type)]
  , name :: Text
} deriving (Show, Eq)
