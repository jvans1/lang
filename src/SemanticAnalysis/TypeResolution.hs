{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module SemanticAnalysis.TypeResolution(assignTypes) where
import Control.Monad.State.Lazy(MonadState, State, evalState, withState, get, put, modify)
import Debug.Trace(trace)
import qualified Data.Text as T
import Control.Monad.Extra(findM)
import Control.Monad.Reader(ask)
import Data.Foldable(foldrM)
import qualified Data.List.NonEmpty as NE
import Types
import Control.Monad.Writer.Lazy(tell, runWriter, Writer, runWriterT)
import Control.Monad.State.Lazy(evalState)
import Control.Monad.State.Lazy(get)
import Data.HashMap.Strict(mapWithKey)
import Base hiding (mapWithKey)
import Control.Monad.Writer.Lazy(MonadWriter, WriterT, runWriterT)

assignTypes :: HashMap Text (Type, Expr) ->  Either [TypeError] Program
assignTypes exprs = case runTypeAssignment exprs of
                          (pgrm, [])  -> Right pgrm
                          (_, xs)     -> Left xs


typeAssignment :: TypeAssignment Program
typeAssignment = get >>= foldrM addExplicitTypes empty

newtype TypeAssignment a = TypeAssignment {
  _runTypeAssignment :: WriterT [TypeError] (State Scope) a
} deriving (Functor, Applicative, Monad, MonadState Scope, MonadWriter [TypeError])

runTypeAssignment :: HashMap Text (Type, Expr) -> (Program, [TypeError])
runTypeAssignment scope = flip evalState scope (runWriterT (_runTypeAssignment typeAssignment))

addExplicitTypes :: (Type, Expr) -> Program -> TypeAssignment Program
addExplicitTypes expr prgm = do
  mfn <- addFnType expr
  case mfn of
    Just fn -> return $ insert (typedFnName fn) fn prgm
    Nothing -> return prgm

makeTypedFn :: Expr -> [Maybe TypedExpr] -> [Maybe TypedExpr] -> Maybe Type -> Maybe TypedFunction
makeTypedFn Function{..} margs mbody mretType = do
  let (returnStatement, mlist) = NE.uncons fnbody
  args <- sequence margs
  body <- sequence mbody
  rType <- mretType
  return TypedFunction {
        retStatement = (rType, returnStatement)
        , typedFnName = fnName
        , tyfnArgTypes = fnArgTypes
        , tyRetType = retType
        , tyFnArgs = args
        , tyFnbody = body
     }

withLocalScope :: [TypedExpr] -> TypeAssignment a -> TypeAssignment a 
withLocalScope localScope expr = do
  oldState <- get
  modify (addArgs localScope)
  result <- expr
  put oldState
  return result


addFnType :: (Type, Expr) -> TypeAssignment (Maybe TypedFunction)
addFnType (ftype, fn@Function{..}) = do
  let (returnStatement, mlist) = NE.uncons fnbody
  withLocalScope (zip fnArgTypes fnArgs) $ do
    mtypedFnArgs         <- mapM assignType fnArgs
    mfbody               <- fnBody mlist
    mrType <-  typeOf returnStatement
    return $ makeTypedFn fn mtypedFnArgs mfbody mrType
--This shouldn't happen, lets enforce this on the type level
addFnType _ = tell [InvalidEntry] >> return Nothing


addArgs :: [TypedExpr] -> Scope -> Scope
addArgs exprs oldState = foldl' addToScope oldState exprs 
  where
    addToScope :: Scope -> TypedExpr -> Scope
    addToScope scope tExpr@(_, Var name _) = insert name tExpr scope

fnBody :: Maybe (NonEmpty Expr) -> TypeAssignment [Maybe TypedExpr]
fnBody (Just a) = mapM assignType $ NE.toList a
fnBody Nothing  = return []

assignType :: Expr -> TypeAssignment (Maybe TypedExpr)
assignType ex = do
    mtype <- typeOf ex 
    case mtype of
      Just ttype -> return $ Just (ttype, ex)
      Nothing -> return Nothing

typeOf :: Expr -> TypeAssignment (Maybe Type)
typeOf ex@(Assignment expr1 expr2 _) = typeOf expr2
typeOf ex@(Function{..})             = return (Just retType)
typeOf ex@(Lit (StringLit _) _)      = return (Just String)
typeOf ex@(Lit (Digit _) _)          = return (Just Integer)
typeOf ex@(Var name lex)               = do
  mType <- fmap fst . lookup name <$> get
  case mType of
    Nothing -> tell [UndefinedVariable name ex] >> return Nothing
    a -> return a
typeOf ex@(Call name _ _)            = typeOfFn name ex

typeOfFn :: Text -> Expr -> TypeAssignment (Maybe Type)
typeOfFn name ex = do
  scope <- get
  let mfn = lookup name scope
  case mfn of
    Just (_, fn) -> typeOf fn
    Nothing -> tell [UnknownFunction name (location ex)] >> return Nothing
