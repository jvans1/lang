{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module SemanticAnalysis.TypeResolution(fn0) where
import qualified Data.Text as T
import Control.Monad.Reader(ask)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty(..))
import Types
import Control.Monad.Writer.Lazy(tell, runWriter, Writer, runWriterT)
import Control.Monad.Reader(MonadReader)
import Base hiding (mapWithKey)
import Control.Monad.Writer.Lazy(MonadWriter, WriterT, runWriterT)
import Data.Maybe(fromJust)

toProg :: HashMap Text (Maybe TypedFunction) -> HashMap Text TypedFunction
toProg = fmap fromJust

fn0 :: HashMap Text (Type, Expr) ->  Either [TypeError] Program
fn0 exprs = case runWriter (fnn exprs) of
              (prg, []) -> Right (toProg prg)
              (_, xs) ->   Left xs

fnn :: HashMap Text (Type, Expr) -> Writer [TypeError] (HashMap Text (Maybe TypedFunction))
fnn fns = mapM (fn1 fns) fns 

fn1 :: Scope -> (Type, Expr) -> Writer [TypeError] (Maybe TypedFunction) 
fn1 scope expr = runReaderT (assignFnType expr) scope

assignFnType :: (Type, Expr) -> ReaderT Scope (Writer [TypeError]) (Maybe TypedFunction)
assignFnType (ftype, Function{..}) = do
  mbody <- sequence <$> mapM typeOf fnbody
  case mbody of
    Just all@(a :| xs) -> do
      return . Just $ TypedFunction {
        retStatement = a
        , typedFnName = fnName
        , tyRetType = retType
        , tyFnbody = a:xs
        , tyFnArgs = error "tyFnArgs"
      }
    Nothing -> return Nothing

typeOf :: Expr -> ReaderT Scope (Writer [TypeError]) (Maybe (Type, Expr))
typeOf ex@(Assignment expr1 expr2 _) = typeOf expr2
typeOf ex@(Function{..})             = return $ Just (retType, ex)
typeOf ex@(Lit (StringLit _) _)      = return $ Just (String, ex)
typeOf ex@(Lit (Digit _) _)          = return $ Just (Integer, ex)
typeOf ex@(Var name lex)               = do
  mType <- fmap fst . lookup name <$> ask
  case mType of
    Nothing -> tell [UndefinedVariable name ex] >> return Nothing
    Just a -> return $ Just (a, ex)
typeOf ex@(Call name _ _)            = typeOfFn name ex

typeOfFn :: Text -> Expr -> ReaderT Scope (Writer [TypeError]) (Maybe (Type, Expr))
typeOfFn name ex = do
  scope <- ask
  let mfn = lookup name scope
  case mfn of
    Just (_, fn) -> typeOf fn
    Nothing -> tell [UnknownFunction name (location ex)] >> return Nothing
