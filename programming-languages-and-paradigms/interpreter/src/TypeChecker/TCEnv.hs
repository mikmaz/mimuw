module TypeChecker.TCEnv where

    import Data.Maybe
    import Data.Map as Map

    import Control.Monad.Reader
    import Control.Monad.Except

    import Grammar.AbsCPC

    import TypeChecker.Exception

    type Scope = Integer
    type RetType = Maybe Type
    type ScopeType = (Scope, Type)
    type TCEnv = (Map.Map String ScopeType, Scope, RetType, Bool)

    type TC = ReaderT TCEnv (ExceptT PosTCExc IO)

    initTCEnv :: TCEnv
    initTCEnv = (Map.empty, 0, Nothing, False)
