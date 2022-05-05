module Interpreter.Eval where

    import qualified Data.Map as Map
    import Data.Maybe

    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except

    import Grammar.AbsCPC

    import Interpreter.Env
    import Interpreter.Exception
    import Interpreter.Util
    

    evalRelOp :: Integer -> RelOp -> Integer -> Bool
    evalRelOp e1 (GTH _) e2 = e1 > e2
    evalRelOp e1 (GE _) e2 = e1 >= e2
    evalRelOp e1 (LTH _) e2 = e1 < e2
    evalRelOp e1 (EQU _) e2 = e1 == e2
    evalRelOp e1 (NE _) e2 = e1 /= e2
    evalRelOp e1 (LE _) e2 = e1 <= e2


    evalMulOp :: Integer -> MulOp -> Integer -> Integer
    evalMulOp e1 (Div _) e2 = div e1 e2
    evalMulOp e1 (Times _) e2 = e1 * e2
    evalMulOp e1 (Mod _) e2 = e1 `mod` e2


    evalAddOp :: Integer -> AddOp -> Integer -> Integer
    evalAddOp e1 (Minus _) e2 = e1 - e2
    evalAddOp e1 (Plus _) e2 = e1 + e2
    

    storeFunArgVals :: [Expr] -> [Arg] -> Env -> Eval Env    
    storeFunArgVals [] [] _ = ask
    storeFunArgVals (expr:exprs) ((Arg _ t var):args) exprArgsEnv = do
        cpcVal <- local (const exprArgsEnv) (evalExpr expr)
        env <- storeIdent var cpcVal
        local (const env) (storeFunArgVals exprs args exprArgsEnv)


    evalExpr :: Expr -> Eval CPCVal
    evalExpr (EVar _ var) = readIdentVal var

    evalExpr (ELitInt _ i) = return (CPCInt i)

    evalExpr (ELitTrue _) = return (CPCBool True)

    evalExpr (ELitFalse _) = return (CPCBool False)

    evalExpr (EApp _ fname argExprs) = do
        exprArgsEnv <- ask
        (CPCFun (fretType, args, fbody, fdeclEnv)) <- readIdentVal fname
        fbodyEnv <- local (const fdeclEnv) (storeFunArgVals argExprs args exprArgsEnv)
        (_, fret) <- local (const fbodyEnv) (evalStmts fbody)
        case fret of
            Nothing -> return CPCVoid
            (Just cpcVal) -> return cpcVal
  
    evalExpr (EString _ str) = return (CPCStr str)

    evalExpr (Neg _ expr) = do
        (CPCInt i) <- evalExpr expr
        return (CPCInt (-i))
    
    evalExpr (Not _ expr) = do
        (CPCBool b) <- evalExpr expr
        return (CPCBool (not b))

    evalExpr (EMul _ e1 operator e2) = do
        (CPCInt i1) <- evalExpr e1
        (CPCInt i2) <- evalExpr e2
        if i2 == 0 then
            case operator of
                (Div _) -> throwError (hasPosition e2, DivisionByZero)
                (Mod _) -> throwError (hasPosition e2, ModulusByZero)
                _ -> return (CPCInt (evalMulOp i1 operator i2))
        else
            return (CPCInt (evalMulOp i1 operator i2))
    
    evalExpr (EAdd _ e1 operator e2) = do
        CPCInt i1 <- evalExpr e1
        CPCInt i2 <- evalExpr e2
        return (CPCInt (evalAddOp i1 operator i2))

    evalExpr (ERel _ e1 operator e2) = do
        CPCInt i1 <- evalExpr e1
        CPCInt i2 <- evalExpr e2
        return (CPCBool (evalRelOp i1 operator i2))
    
    evalExpr (EAnd _ e1 e2) = do
        CPCBool b1 <- evalExpr e1
        CPCBool b2 <- evalExpr e2
        return (CPCBool (b1 && b2))

    evalExpr (EOr _ e1 e2) = do
        CPCBool b1 <- evalExpr e1
        CPCBool b2 <- evalExpr e2
        return (CPCBool (b1 || b2))

    evalExpr (ELitTuple _ exprs) = do
        tupleVals <- evalTupleLit exprs
        return (CPCTuple tupleVals)

    evalTupleLit :: [Expr] -> Eval [CPCVal]
    evalTupleLit [] = return []
    evalTupleLit (expr:exprs) = do
        cpcVals <- evalTupleLit exprs
        cpcVal <- evalExpr expr
        return (cpcVal:cpcVals)


    storeDeclVars :: Type -> [Item] -> Eval Env
    storeDeclVars varsType [] = ask
    storeDeclVars varsType (item:items) = do
        case item of
            (NoInit _ (Ident varName)) -> do
                env <- storeIdent (Ident varName) (defaultTypeVal varsType)
                local (const env) (storeDeclVars varsType items)
            (Init _ (Ident varName) expr) -> do
                cpcVal <- evalExpr expr
                env <- storeIdent (Ident varName) cpcVal
                local (const env) (storeDeclVars varsType items)


    getPrintStr :: [Expr] -> Eval String
    getPrintStr [] = return "\n"
    getPrintStr (expr:exprs) = do
        resStr <- getPrintStr exprs
        cpcVal <- evalExpr expr
        return (cpcValToStr cpcVal ++ " " ++ resStr)
    

    evalStmt :: Stmt -> Eval (Env, RetVal)
    evalStmt (Empty _) = do
        env <- ask
        return (env, Nothing)

    evalStmt (BStmt _ (Block _ stmts)) = do
        env <- ask
        (_, ret) <- evalStmts stmts
        return (env, ret)

    evalStmt (Decl _ varsType vars) = do
        env <- storeDeclVars varsType vars
        return (env, Nothing)

    evalStmt (Ass _ var expr) = do
        cpcVal <- evalExpr expr
        updateIdentVal var cpcVal
        env <- ask
        return (env, Nothing)

    evalStmt (FnDef _ fretType (Ident fname) args (Block _ stmts)) = do
        env <- storeIdent (Ident fname) CPCVoid
        local (const env) (updateIdentVal (Ident fname) (CPCFun (fretType, args, stmts, env)))
        return (env, Nothing)
    
    evalStmt (Incr _ var) = do
        CPCInt varVal <- readIdentVal var
        updateIdentVal var (CPCInt (varVal + 1))
        env <- ask
        return (env, Nothing)

    evalStmt (Decr _ var) = do
        CPCInt varVal <- readIdentVal var
        updateIdentVal var (CPCInt (varVal - 1))
        env <- ask
        return (env, Nothing)

    evalStmt (Ret _ expr) = do
        cpcRetVal <- evalExpr expr
        env <- ask
        return (env, Just cpcRetVal)
    
    evalStmt (VRet _) = do
        env <- ask
        return (env, Just CPCVoid)
    
    evalStmt (Cond _ ifExpr (Block _ body)) = do
        CPCBool condVal <- evalExpr ifExpr
        env <- ask
        if condVal then do
            (_, ret) <- evalStmts body
            return (env, ret)
        else
            return (env, Nothing)
    
    evalStmt (CondElse _ ifExpr (Block _ ifBody) (Block _ elseBody)) = do
        CPCBool condVal <- evalExpr ifExpr
        env <- ask
        if condVal then do
            (_, ret) <- evalStmts ifBody
            return (env, ret)
        else do
            (_, ret) <- evalStmts elseBody
            return (env, ret)

    evalStmt (While pos expr (Block bpos body)) = do
        CPCBool whileCond <- evalExpr expr
        env <- ask
        if whileCond then do
            (_, ret) <- evalStmts body
            case ret of
                Nothing -> evalStmt (While pos expr (Block bpos body))
                _ -> return (env, ret)
        else return (env, Nothing)
    
    evalStmt (SExp _ expr) = do
        _ <- evalExpr expr
        env <- ask
        return (env, Nothing)

    evalStmt (Print _ exprs) = do
        strToPrint <- getPrintStr exprs
        liftIO (putStr strToPrint)
        env <- ask
        return (env, Nothing)

    evalStmt (AssTuple _ tElems expr) = do
        CPCTuple exprVals <- evalExpr expr
        updateTupleIdents tElems exprVals
        env <- ask
        return (env, Nothing)

    evalStmt (For _ iter fromExpr toExpr (Block _ body)) = do
        (CPCInt fromVal) <- evalExpr fromExpr
        (CPCInt toVal) <- evalExpr toExpr
        env <- ask
        if fromVal <= toVal then do
            forEnv <- storeIdent iter (CPCInt fromVal)
            local (const forEnv) (runForLoop iter toVal body env)
        else do
            return (env, Nothing)

    updateTupleIdents :: [Tuple] -> [CPCVal] -> Eval ()
    updateTupleIdents [] [] = return ()
    updateTupleIdents (t:ts) (v:vs) = case (t, v) of
        (Tuple _ tupleIdents, CPCTuple tupleVals) -> do
            updateTupleIdents tupleIdents tupleVals
            updateTupleIdents ts vs
        (TupleElem _ ident, _) -> do
            updateIdentVal ident v
            updateTupleIdents ts vs

    runForLoop :: Ident -> Integer -> [Stmt] -> Env -> Eval (Env, RetVal)
    runForLoop (Ident iter) toIntVal body retEnv = do
        (_, ret) <- evalStmts body
        case ret of 
            Nothing -> do
                (CPCInt i) <- readIdentVal (Ident iter)
                if i < toIntVal then do
                    updateIdentVal (Ident iter) (CPCInt (i + 1))
                    runForLoop (Ident iter) toIntVal body retEnv
                else
                    return (retEnv, Nothing)
            _ -> return (retEnv, ret)


    evalStmts :: [Stmt] -> Eval (Env, RetVal)
    evalStmts [] = do
        env <- ask
        return (env, Nothing)
    evalStmts (stmt:stmts) = do
                (env, ret) <- evalStmt stmt
                if isNothing ret then
                    local (const env) (evalStmts stmts)
                else
                    return (env, ret)


    evalTopDef :: TopDef -> Eval (Env, RetVal)
    evalTopDef (TopFnDef _ fretType (Ident fname) args (Block _ stmts)) = do
        env <- storeIdent (Ident fname) CPCVoid
        local (const env) (updateIdentVal (Ident fname) (CPCFun (fretType, args, stmts, env)))
        return (env, Nothing)


    evalTopDefs :: [TopDef] -> Eval (Env, RetVal)
    evalTopDefs [] = do
        env <- ask
        return (env, Nothing)
    evalTopDefs ((TopFnDef pos fretType (Ident fname) args (Block bpos stmts)):tds) = do
        if fname == "main" then 
            evalStmts stmts
        else do
            (env, ret) <- evalTopDef (TopFnDef pos fretType (Ident fname) args (Block bpos stmts))
            local (const env) (evalTopDefs tds)


    evalProgram :: [TopDef] -> IO (Either PosRuntimeExc ((Env, RetVal), Store))
    evalProgram program = runExceptT $ runStateT (runReaderT (evalTopDefs program) initEnv) initStore
