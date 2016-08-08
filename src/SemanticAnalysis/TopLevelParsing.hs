{-# LANGUAGE RecordWildCards #-}
module SemanticAnalysis.TopLevelParsing(parseTop) where
import Types
import Data.Foldable(foldrM)
import Base
import Control.Monad.Writer.Lazy(tell, runWriter, Writer, runWriterT)

parseTop :: [Expr] -> Either [TypeError] (HashMap Text (Type, Expr))
parseTop exprs = 
  case runWriter (assignFnTypes exprs) of
    (fncts, []) -> Right fncts
    (_, xs)     -> Left xs



assignFnTypes :: [Expr] -> Writer [TypeError] (HashMap Text (Type, Expr))
assignFnTypes exprs = foldrM addTopLevelDeclarations empty $ reverse exprs 
  where
    addTopLevelDeclarations :: Expr -> HashMap Text (Type, Expr) -> Writer [TypeError] (HashMap Text (Type, Expr))
    addTopLevelDeclarations fn@Function{..} hmap = do
      case lookup fnName hmap of
        Just _ -> tell [DuplicateDeclaration fnName fn] >> return hmap
        Nothing -> return $ insert fnName (retType, fn) hmap
    addTopLevelDeclarations expr hmap = tell [NakedExpression expr] >> return hmap
  
