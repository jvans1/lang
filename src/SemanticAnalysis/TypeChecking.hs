{-# LANGUAGE RecordWildCards #-}

module SemanticAnalysis.TypeChecking(typeCheck) where
import Control.Monad.Writer.Lazy(tell, runWriter, Writer)
import SemanticAnalysis.TypeResolution
import Control.Monad.Writer.Strict(tell)
import SemanticAnalysis.TopLevelParsing(parseTop)
import Types
import Base

typeCheck :: [Expr] -> Either [TypeError] Program
typeCheck exprs = parseTop exprs >>= fn0 >>= runTypeChecker

logTypeMismatch :: Type -> TypedExpr -> Writer [TypeError] ()
logTypeMismatch type1 typedExpr@(type2, expr) = if type1 /= type2 then
                                        tell $ [MisMatch type1 typedExpr]
                                      else return ()

logTypeErrors :: Program -> Writer [TypeError] Program
logTypeErrors prgm = do
  forM_ prgm $ \TypedFunction {..} -> do
    logTypeMismatch tyRetType retStatement
  return prgm

runTypeChecker :: Program -> Either [TypeError] Program
runTypeChecker prgm = case runWriter (logTypeErrors prgm) of
                          (prgm, [])  ->  return prgm
                          (_,  errs)  ->  Left errs
