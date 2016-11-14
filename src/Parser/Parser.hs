{-# LANGUAGE RecordWildCards #-}
module Parser.Parser where
import qualified Data.List.NonEmpty as NE
import Base
import Text.Parsec(try, alphaNum, char, oneOf, letter, Parsec, many1, count, sepBy1, manyTill, many, space, optionMaybe, string, sepBy, newline, spaces)
import Types
import Parser.Lexer(lexer)
import Data.Functor((<$))
import Control.Applicative((*>), (<*))
import Text.Parsec(sourceLine, sourceName, getPosition)
import Text.Parsec.Language(emptyDef)
import qualified Text.Parsec as P
import Control.Applicative((<|>))
import Text.Parsec.Token(GenLanguageDef, GenTokenParser, makeTokenParser)
import qualified Text.Parsec.Token as Tok-- (GenLanguageDef(..), GenTokenParser(..), makeTokenParser)
{-
  Grammar
  Expr       -> Def | Call | Assignment | Term
  Assignment -> Var = Expr
  Term       -> Int | Var
-}

parse :: String -> Either P.ParseError [Expr]
parse expr = P.parse (many1 functionDeclaration) "" expr

expression :: Parsec String () Expr
expression = try assignment <|> factor

factor :: Parsec String () Expr
factor = try functionCall <|> term

term :: Parsec String () Expr
term = variable <|> digit <|> stringLiteral

getLexeme :: Parsec String () Lexeme
getLexeme  = liftM2 Lexeme (sourceLine <$> getPosition) (pack . sourceName <$> getPosition)

stringLiteral :: Parsec String () Expr
stringLiteral = do
  lex <- getLexeme
  lit <- pack <$> Tok.stringLiteral lexer
  return $ Lit (StringLit lit) lex


symbol :: String -> Parsec String () String
symbol = Tok.symbol lexer

parens :: Parsec String () a -> Parsec String () a
parens = Tok.parens lexer

commaSep :: Parsec String () a -> Parsec String () [a]
commaSep = Tok.commaSep lexer

addition :: Parsec String () Expr
addition = do
  lex <- getLexeme
  expr1 <- term
  spaces
  symbol "+"
  expr2 <- expression
  return $ Call "+" [expr1, expr2] lex

functionCall :: Parsec String () Expr
functionCall = try namedFunction
  where
    namedFunction :: Parsec String () Expr
    namedFunction = do
      lex <- getLexeme
      fnName <- identifier
      fnArgs <- parens $ commaSep factor
      return (Call fnName fnArgs lex)

functionDeclaration :: Parsec String () Expr
functionDeclaration = do
  lex <- getLexeme
  Tok.reserved lexer "tydef"
  name <- identifier
  atypes <- argTypes
  Tok.reservedOp lexer "=>"
  rt <- ftype
  newline
  Tok.reservedOp lexer "fndef"
  fnname <- identifier
--TODO Check fn names match
  varargs <- parens varArguments
  body <- NE.fromList <$> (many space *> many1 expression <* many space)
  Tok.reservedOp lexer "end"
  many space
  return $ Function name atypes rt varargs body lex

varArguments :: Parsec String () [Expr]
varArguments = sepBy variable (symbol ",")

ftype :: Parsec String () Type
ftype = intType <|> strType
  where
    intType ::  Parsec String () Type
    intType = Integer      <$ string "Integer"
    strType ::  Parsec String () Type
    strType = String <$ string "String"

argTypes :: Parsec String () [Type]
argTypes = parens ftypes

ftypes :: Parsec String () [Type]
ftypes = commaSep ftype

assignment :: Parsec String () Expr
assignment = do
  lex <- getLexeme
  var <- variable
  Tok.reservedOp lexer "="
  assigned <- expression
  return (Assignment var assigned lex)

digit :: Parsec String () Expr
digit = do
  lex <- getLexeme
  digits <-  read <$> many1 P.digit
  return $ Lit (Digit digits) lex

identifier :: Parsec String () Text
identifier = pack <$> Tok.identifier lexer

variable :: Parsec String () Expr
variable = do
  lex <- getLexeme
  var <- identifier
  return $ Var var lex
