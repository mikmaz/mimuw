module Main where

    import System.IO ( hPutStrLn, stderr, getContents )
    import System.Environment ( getArgs )
    import System.Exit ( exitFailure, exitSuccess, exitWith, ExitCode( ExitFailure ) )

    import Grammar.AbsCPC
    import Grammar.ParCPC

    import TypeChecker.TypeCheck
    import TypeChecker.Exception

    import Interpreter.Env
    import Interpreter.Eval
    import Interpreter.Exception
    
    msgExitFailure :: String -> IO ()
    msgExitFailure msg = do
        hPutStrLn stderr msg
        exitFailure

    runProgram :: String -> IO ()
    runProgram progStr = case pProgram (myLexer progStr) of
        Right progTree -> do
            let Program _ program = progTree
            typeCheckResult <- runTypeChecker program
            case typeCheckResult of
                    Left tcExc -> msgExitFailure $ getTcErrMsg tcExc
                    Right _ -> do
                        runtimeRes <- evalProgram program
                        case runtimeRes of
                            Left runtimeExc -> msgExitFailure $ getRuntimeErrMsg runtimeExc
                            Right ((_, Just (CPCInt retVal)), _) -> 
                                if retVal == 0 then
                                    exitSuccess
                                else
                                    exitWith (ExitFailure (fromInteger retVal))
        Left s -> msgExitFailure s

    loadFile :: String -> IO ()
    loadFile filename = do 
        fileStr <- readFile filename
        runProgram fileStr

    main :: IO ()
    main = do
        filenames <- getArgs
        case filenames of
            [] -> do  
                inStr <- getContents
                runProgram inStr
            (filename:[]) -> loadFile filename
            _ -> msgExitFailure "Usage: interpreter program\nUsage: interpreter"
