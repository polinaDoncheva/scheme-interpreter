module Repl where

import Expr ()
import Env ( Env )
import Eval ( eval )
import Parser ( parseInput )
import System.IO (hFlush, stdout)


-- Repl

repl :: Env -> IO ()
repl env = do
    putStr "scheme> "
    hFlush stdout
    input <- getLine
    if input == "exit"
        then putStrLn "Goodbye!"
        else do
            let parsed = parseInput input
            case parsed of
                Left err -> do
                    putStrLn $ "Error: " ++ err
                    repl env
                Right expr -> do
                    let result = eval env expr
                    case result of
                        Left err -> do
                            putStrLn $ "Error: " ++ err
                            repl env
                        Right (newEnv, val) -> do
                            print val
                            repl newEnv


-- Repl helper

printEnv :: Env -> IO ()
printEnv env = do
    putStrLn "Current Environment:"
    mapM_ (\(var, expr) -> putStrLn $ var ++ " = " ++ show expr) env
