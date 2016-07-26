module Parser.Lexer(lexer) where
import Text.Parsec.Token(GenLanguageDef(..), GenTokenParser(..), makeTokenParser)
import Data.Functor.Identity(Identity)
import Text.Parsec.Language(emptyDef)

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser languageDef

languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef {
   caseSensitive = True
  , reservedNames =  ["fndef", "tydef", "end"]
  , reservedOpNames =  ["=>", "+"]
}
