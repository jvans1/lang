{-# LANGUAGE RecordWildCards #-}

module SemanticAnalysis.TypeChecking where
import SemanticAnalysis.TypeResolution
import Control.Monad.Writer.Strict(tell)
import Types
import Base

typeCheck :: [Expr] -> Either [TypeError] Program
typeCheck exprs = case parseTop exprs >>= assignTypes of
                      Right p -> case runTypeChecker p typeChecker of
                                    (prgm, [])  ->  return prgm
                                    (_,  errs)  ->  Left errs
                      Left a -> Left a


logTypeMismatch :: Type -> TypedExpr -> TypeChecker ()
logTypeMismatch type1 typedExpr@(type2, expr) = if type1 /= type2 then
                                        tell $ [MisMatch type1 typedExpr]
                                      else return ()

typeChecker :: TypeChecker Program
typeChecker = do
  prgm <- ask
  forM_ prgm $ \TypedFunction {..} -> do
    logTypeMismatch tyRetType retStatement
  return prgm
