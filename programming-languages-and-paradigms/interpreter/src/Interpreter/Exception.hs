module Interpreter.Exception where

    import Grammar.AbsCPC

    type PosRuntimeExc = (BNFC'Position, RuntimeException)

    data RuntimeException
        = DivisionByZero
        | ModulusByZero deriving Show

    showRuntimeExc :: RuntimeException -> String
    showRuntimeExc e = case e of
        DivisionByZero -> "division by zero"
        ModulusByZero -> "modulus by zero"
    
    getRuntimeErrMsg :: PosRuntimeExc -> String
    getRuntimeErrMsg (pos, runtimeExc) = case pos of
        Nothing -> "runtime error: " ++ showRuntimeExc runtimeExc
        Just (l, c) -> "runtime error:" ++ show l ++ ":" ++ show c ++ ": " ++ showRuntimeExc runtimeExc
