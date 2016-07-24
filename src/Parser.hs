module Parser where
import Text.Parsec(alphaNum, char, oneOf, letter, Parsec, many1, count, sepBy1, manyTill, many, space, optionMaybe)
import Data.Functor.Identity(Identity)
import Text.Parsec.Language(emptyDef)
import qualified Text.Parsec as P
import Control.Applicative((<|>))
import Text.Parsec.Token(GenLanguageDef(..), GenTokenParser(..), makeTokenParser)
{- 
  Grammar
  S       -> Types | Ident Func | Term + S | Term
  Types   -> Type | Type => Types
  Type    -> Integer
  Func    -> Params = | Factor
  Params  -> ident | ident Params
  Term    -> Factor * Term | Term / Expr | Factor < Expr | Factor
  Factor  -> Ident | Digit | (Expr) | Ident OptParams
-}
languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef {
   caseSensitive = True
  , reservedNames =  []
  , reservedOpNames =  ["=>"]
}

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser languageDef

data Op = Add | Mult deriving (Show, Eq)
data Type = Integer deriving (Show, Eq, Enum)
data Expr = Function String [Type] Type [Expr] Expr 
            | Var String 
            | BinaryOp Op Expr Expr
            | Digit Integer deriving (Show, Eq)

parse :: String -> Expr
parse xs = case P.parse expression "" xs of
            Right res -> res
            Left err   -> error $ show err

ident :: Parsec String () String
ident = identifier lexer

args :: Parsec String () Expr
args = identifier lexer >>= return . Var

types :: Parsec String () Type
types = do
  res <- symbol lexer "Integer" 
  case res of
    "Integer" -> return Integer

typeArrow :: Parsec String () ()
typeArrow = reservedOp lexer "=>"

fnType :: Parsec String () ([Type], Type)
fnType = do 
  typ <- types
  arrow <- optionMaybe typeArrow
  case arrow of
    Just _ -> do 
      (xs, rt) <- fnType
      return (typ:xs, rt)
    Nothing -> return ([], typ)
  

fnArgs :: Parsec String () [Expr]
fnArgs = manyTill variable (P.string "=")

digit :: Parsec String () Expr
digit = do
  num <- many1 P.digit 
  return (Digit $ read num)

variable :: Parsec String () Expr
variable =  Var <$> ident

term :: Parsec String () Expr
term = factor

factor :: Parsec String () Expr
factor = digit <|> variable

fnBody :: Parsec String () Expr
fnBody = factor
  
function :: Parsec String () Expr
function = do
  idt <- ident
  (argTypes, retType) <- fnType
  args <- fnArgs
  many space
  bdy <- fnBody
  return (Function idt argTypes retType args bdy)

multiplication :: Parsec String () Expr
multiplication = do
  arg2 <- factor
  many space >> reserved lexer "*" >> many space
  arg1 <- term
  return (BinaryOp Mult arg1 arg2)

addition :: Parsec String () Expr
addition = do 
  arg1 <- term
  many space >> reserved lexer "+" >> many space
  arg2 <- expression
  return (BinaryOp Add arg1 arg2)

expression :: Parsec String () Expr
expression = addition <|> term
