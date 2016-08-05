{-# LANGUAGE RecordWildCards #-}
module SemanticAnalysis.TypeResolution(typeCheck) where
import Control.Monad.Extra(findM)
import Control.Monad.Except(throwError)
import Control.Monad.Reader(ask)
import Data.Foldable(foldrM)
import qualified Data.List.NonEmpty as NE
import Types
import Control.Monad.Writer.Lazy(tell, runWriter, Writer)
import Control.Monad.State.Lazy(get)
import Data.HashMap.Strict(mapWithKey)
import Base hiding (mapWithKey)

typeAssignment :: TypeAssignment Program
typeAssignment = get >>= foldrM addExplicitTypes empty

parseTop :: [Expr] -> Writer [TypeError] (HashMap Text (Type, Expr))
parseTop = error "parseTop"

typeCheck :: [Expr] -> Either [TypeError] Program
typeCheck exprs =
  case runWriter (parseTop exprs) of
    (fncts, []) ->  case assignTypes fncts typeAssignment of
                      Right p -> case runTypeChecker p typeChecker of
                                    (prgm, [])  ->  return prgm
                                    (_,  errs)  ->  Left errs
                      Left a -> Left [a]
    (_, xs)    ->  Left xs

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

addExplicitTypes :: (Type, Expr) -> Program -> TypeAssignment Program
addExplicitTypes expr prgm = do
  fn <- addFnType expr
  return $ insert (typedFnName fn) fn prgm

addFnType :: (Type, Expr) -> TypeAssignment TypedFunction
addFnType (ftype, Function{..}) = do
  let (returnStatement, mlist) = NE.uncons fnbody
  typedFnArgs         <- mapM assignType fnArgs
  fbody               <- fnBody mlist
  return $ TypedFunction {
            retStatement = (ftype, returnStatement)
            , typedFnName = fnName
            , tyfnArgTypes = fnArgTypes
            , tyRetType = retType
            , tyFnArgs = typedFnArgs
            , tyFnbody = fbody
         }
addFnType a = error "this shouldn't happen, move to type representation"

fnBody :: Maybe (NonEmpty Expr) -> TypeAssignment [TypedExpr]
fnBody (Just a) = mapM assignType $ NE.toList a
fnBody Nothing  = return []

assignType :: Expr -> TypeAssignment TypedExpr
assignType ex = typeOf ex >>= return . (flip (,) ex)


isNamed :: Text -> Expr -> TypeAssignment Bool
isNamed name Function{..} = return $ fnName == name
--TODO: Account for invalid top level declarations here and short circuit
--If we have a TLD that is not a function this will fail 

typeOf :: Expr -> TypeAssignment Type
typeOf ex@(Assignment expr1 expr2 _) = typeOf expr2
typeOf ex@(Function{..})             = return retType
typeOf ex@(Lit (StringLit _) _)      = return String
typeOf ex@(Lit (Digit _) _)          = return Integer
typeOf ex@(Var _ _)                  = return Integer
typeOf ex@(Call name _ _)            = typeOfFn name ex

typeOfFn :: Text -> Expr -> TypeAssignment Type
typeOfFn name ex = do
  scope <- get
  let mfn = lookup name scope
  case mfn of
    Just (_, fn) -> typeOf fn
    Nothing -> throwError $ UnknownFunction name (location ex)
