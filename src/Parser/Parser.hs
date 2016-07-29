{-# LANGUAGE RecordWildCards #-}
module Parser.Parser where
import qualified Data.List.NonEmpty as NE
import Base
import Text.Parsec(try, alphaNum, char, oneOf, letter, Parsec, many1, count, sepBy1, manyTill, many, space, optionMaybe, string, sepBy, newline, spaces)
import Types
import Parser.Lexer(lexer)
import Data.Functor((<$))
import Control.Applicative((*>), (<*))
import Text.Parsec.Language(emptyDef)
import qualified Text.Parsec as P
import Control.Applicative((<|>))
import Text.Parsec.Token(GenLanguageDef(..), GenTokenParser(..), makeTokenParser)
{-
  Grammar
  Expr       -> Def | Call | Assignment | Term
  Assignment -> Var = Expr
  Term       -> Int | Var
-}

parse :: String -> Either P.ParseError [Expr]
parse expr = P.parse parseTop "" expr

parseTop :: Parsec String () [Expr]
parseTop = many1 functionDeclaration

expression :: Parsec String () Expr
expression = (try assignment) <|> factor

factor :: Parsec String () Expr
factor = try functionCall <|> term

term :: Parsec String () Expr
term = variable <|> digit <|> (StringLiteral . pack <$> stringLiteral lexer )

addition :: Parsec String () Expr
addition = do
  expr1 <- term
  spaces
  symbol lexer "+"
  expr2 <- expression
  return $ Call "+" [expr1, expr2]

functionCall :: Parsec String () Expr
functionCall = try namedFunction <|> operatorFunction
  where
    operatorFunction :: Parsec String () Expr
    operatorFunction = addition

    namedFunction :: Parsec String () Expr
    namedFunction = do
      fnName <- ident
      fnArgs <- parens lexer $ commaSep lexer factor
      return (Call fnName fnArgs)

functionDeclaration :: Parsec String () Expr
functionDeclaration = do
  reserved lexer "tydef"
  name <- ident
  atypes <- argTypes
  reservedOp lexer "=>"
  rt <- ftype
  newline
  reservedOp lexer "fndef"
  fnname <- ident
--TODO Check fn names match
  varargs <- parens lexer varArguments
  body <- NE.fromList <$> (many space *> many1 expression <* many space)
  reservedOp lexer "end"
  return $ Function name atypes rt varargs body

varArguments :: Parsec String () [Expr]
varArguments = sepBy variable (symbol lexer ",")

ftype :: Parsec String () Type
ftype = intType <|> strType
  where
    intType ::  Parsec String () Type
    intType = Integer      <$ string "Integer"
    strType ::  Parsec String () Type
    strType = String <$ string "String"

argTypes :: Parsec String () [Type]
argTypes = parens lexer ftypes

ftypes :: Parsec String () [Type]
ftypes = commaSep lexer ftype

assignment :: Parsec String () Expr
assignment = do
  var <- variable
  reservedOp lexer "="
  assigned <- expression
  return (Assignment var assigned)

digit :: Parsec String () Expr
digit = Digit . read <$> many1 P.digit

ident :: Parsec String () Text
ident = pack <$> identifier lexer

variable :: Parsec String () Expr
variable = Var <$> ident
