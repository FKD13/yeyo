module LanguageInterpreter where

import Types
import LanguageParser

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

type Env = [(String, Value)]

-- runStateT  :: s -> m (a, s)
-- runExceptT :: ExceptT e m a -> m (Either e a)
-- runExcept  :: Except e a -> Either e a

type ErrorState a = StateT Env (ExceptT String IO) a

evaluate :: Value -> ErrorState Value
evaluate EmptyExpr = return EmptyExpr
evaluate (SetExpr (VarValue s) (NumValue x)) = do env <- get

evaluate (SetExpr (VarValue s) (StringValue x))
evaluate (SetExpr (VarValue s) x)

--addToEnv :: Value -> Value -> 
