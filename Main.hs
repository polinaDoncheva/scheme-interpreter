module Main where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Monad (filterM)


-- Main

main :: IO ()
main = repl []


-- Expression

data Expr
    = Symbol String
    | Number Int
    | SBool Bool
    | Str String
    | List [Expr]
    | Lambda [String] Expr
    | Null
    deriving (Show, Eq)

-- instance Show Expr where
--     show :: Expr -> String
--     show (Symbol s)     = s
--     show (Number n)     = show n
--     show (SBool b)      = if b then "#t" else "#f"
--     show (Str s)        = "\"" ++ s ++ "\""
--     show (List exprs)   = "'(" ++ unwords (map show exprs) ++ ")"
--     show (Lambda args body) = "(lambda (" ++ unwords args ++ ") " ++ show body ++ ")"
--     show Null           = ""


-- Environment

type Env = [(String, Expr)]


-- Evaluator

eval :: Env -> Expr -> Either String (Env,Expr)
eval env (Symbol s) = do
    case lookup s env of
        Just val -> Right (env, val)
        Nothing  -> Left $ "Unbound variable: " ++ s
eval env (Number n) = Right (env, Number n)
eval env (SBool b) = Right (env, SBool b)
eval env (Str s) = Right (env, Str s)
eval env (List [Symbol "define", Symbol var, val]) = do
    (newEnv, evaluated) <- eval env val
    let updatedEnv = (var, evaluated) : newEnv
    Right (updatedEnv, Null)
eval env (List [Symbol "define", List (Symbol name : args), body]) = do
    let argNames = mapM extractSymbol args
    case argNames of
        Left err -> Left err
        Right names -> do
            let updatedEnv = (name, Lambda names body) : env
            Right (updatedEnv, Null)
eval env (List [Symbol "if", cond, thenExpr, elseExpr]) = do
    (newEnv, result) <- eval env cond
    if result == SBool True
        then eval newEnv thenExpr
        else eval newEnv elseExpr
eval env (List (Symbol "cond" : rest)) = evalCond env rest
eval env (List [Symbol "'", args]) = Right (env, args)
eval env (List [Symbol "lambda", List args, body]) =
    let argNames = [s | Symbol s <- args]
    in Right (env, Lambda argNames body)
eval env (List [Symbol "map", funct, rest]) = do
    (_, evaledLambda) <- eval env funct
    case evaledLambda of
        Lambda params body -> do
            (_, evaledList) <- eval env rest
            case evaledList of
                List ls -> do
                    mappedValues <- mapM (evalMapLambda env params body) ls
                    Right (env, List mappedValues)
                _ -> Left "Second argument of map must be list"
        _ -> Left "First argument of map must be a lambda or a function defined by the user"
eval env (List [Symbol "filter", funct, rest]) = do
    (_, evaledLambda) <- eval env funct
    case evaledLambda of
        Lambda params body -> do
            (_, evaledList) <- eval env rest
            case evaledList of
                List ls -> do
                    filteredValues <- filterM (evalFilterPredicate env params body) ls
                    Right (env, List filteredValues)
                _ -> Left "Second argument of filter must be list"
        _ -> Left "First argument of filter must be lambda or a function defined by the user"
eval env (List [Symbol "interpret", Symbol name]) =
    case lookup name env of
        Just expr -> do
            (_, result) <- eval env expr
            Right (env, result)
        Nothing -> Left $ "Symbol '" ++ name ++ "' not found in the environment."
eval env (List [Symbol "or", e1, e2]) = do
    (_, evaledFirst) <- eval env e1
    case evaledFirst of
        SBool True -> Right (env, SBool True)
        SBool False -> do
            (_, evaledSecond) <- eval env e2
            case evaledSecond of
                SBool _ -> Right (env, evaledSecond)
                _       -> Left "Second operand of operator 'or' should evaluate to bool."
        _ -> Left "First operand of operator 'or' should evaluate to bool."
eval env (List [Symbol "and", e1, e2]) = do
    (_, evaledFirst) <- eval env e1
    case e1 of
        SBool False -> Right (env, SBool False)
        SBool True -> do
            (_, evaledSecond) <- eval env e2
            case evaledSecond of
                SBool _ -> Right (env, evaledSecond)
                _       -> Left "Second operand of operator 'and' should evaluate to bool."
        _ -> Left "First operand of operator 'and' should evaluate to bool."
eval env (List (Symbol op : args)) = do
    evaledArgs <- mapM (eval env) args
    let evaledExprs = map snd evaledArgs
    let finalEnv = foldl (\acc (env', _) -> acc ++ env') env evaledArgs
    case op of
        "+"         -> evalArithmetic (+) finalEnv evaledExprs
        "-"         -> evalArithmetic (-) finalEnv evaledExprs
        "*"         -> evalArithmetic (*) finalEnv evaledExprs
        "quotient"  -> evalArithmetic div finalEnv evaledExprs
        "remainder" -> evalArithmetic mod finalEnv evaledExprs
        "cons"      -> evalCons finalEnv evaledExprs
        "car"       -> evalCar finalEnv evaledExprs
        "cdr"       -> evalCdr finalEnv evaledExprs
        "list"      -> evalList finalEnv evaledExprs
        "null?"     -> evalNull finalEnv evaledExprs
        "concat"    -> evalConcat finalEnv evaledExprs
        "="        -> evalEqual finalEnv evaledExprs
        "not"       -> evalNot finalEnv evaledExprs
        "append"    -> evalAppend finalEnv evaledExprs
        "length"    -> evalLength finalEnv evaledExprs
        "reverse"   -> evalReverse finalEnv evaledExprs
        "member"    -> evalMember finalEnv evaledExprs
        _     -> case lookup op env of
            Just (Lambda paramNames body) -> eval finalEnv (List (Lambda paramNames body : evaledExprs))
            _ -> Left $ "Unknown operator: " ++ op
eval env (List (Lambda paramsName body : args)) = do
    evaledArgs <- mapM (eval env) args
    let evaledExprs = map snd evaledArgs
    newEnv <- bindParams paramsName evaledExprs env
    res <- eval newEnv body
    Right (env, snd res)
eval env (List (List first : rest)) = do
    (env', evaluatedFirst) <- eval env (List first)
    let newList = List (evaluatedFirst : rest)
    eval env' newList
eval env (List (Null : rest)) = eval env $ List rest
eval env (List [expr]) = eval env expr
eval env (List []) = Right (env, List [])
eval _ arg = do
    let errorMsg = "Unknow behavior for : " ++ show arg
    Left errorMsg


-- Evaluator helper

evalArithmetic :: (Int -> Int -> Int) -> Env -> [Expr] -> Either String (Env, Expr)
evalArithmetic op env [Number x, Number y] = Right (env, Number $ x `op` y)
evalArithmetic _ _ _ = Left "Invalid arguments for arithmetic operation"

evalCons :: Env -> [Expr] -> Either String (Env, Expr)
evalCons env [x, List xs] = Right (env, List (x : xs))
evalCons _ _ = Left "Invalid arguments for cons"

evalCar :: Env -> [Expr] -> Either String (Env, Expr)
evalCar env [List (x : _)] = Right (env, x)
evalCar _ _ = Left "Invalid arguments for car"

evalCdr :: Env -> [Expr] -> Either String (Env, Expr)
evalCdr env [List (_ : xs)] = Right (env, List xs)
evalCdr _ _ = Left "Invalid arguments for cdr"

evalList :: Env -> [Expr] -> Either String (Env, Expr)
evalList env args = Right (env, List args)

evalNull :: Env -> [Expr] -> Either String (Env, Expr)
evalNull env [Str s] = Right (env, SBool $ null s)
evalNull env [List xs] = Right (env, SBool $ null xs)
evalNull _ _ = Left "Invalid arguments for operator null?, expected a single list or a string."

evalConcat :: Env -> [Expr] -> Either String (Env, Expr)
evalConcat env args = do
    extracted <- mapM extractSymbol args
    Right (env, Str (concat extracted))

evalEqual :: Env -> [Expr] -> Either String (Env, Expr)
evalEqual env [x, y] = Right (env, SBool (equalExprs x y))
evalEqual _ _ = Left "Operator '=' expects exactly two arguments."

evalNot :: Env -> [Expr] -> Either String (Env, Expr)
evalNot env [SBool b] = Right (env, SBool (not b))
evalNot _ _ = Left "Operator not expects exactly one boolean argument."

evalAppend :: Env -> [Expr] -> Either String (Env, Expr)
evalAppend env [List l1, List l2] = Right (env, List $ l1 ++ l2)
evalAppend _ _ = Left "Opertor 'append' expects 2 list operands."

evalCond :: Env -> [Expr] -> Either String (Env, Expr)
evalCond env [List [Symbol "else", body]] = eval env body
evalCond env (List [cond, body] :rest) = do
    (_, evaledCond) <- eval env cond
    case evaledCond of
        SBool True -> eval env body
        SBool False -> evalCond env rest
        _ -> Left "Operator 'cond' expects expression to evaluate to boolean "
evalCond _ _ = Left "Wrong syntax for operator 'cond'"

evalLength :: Env -> [Expr] -> Either String (Env, Expr)
evalLength env [List xs] = Right (env, Number (length xs))
evalLength _ arg = Left $ "Operator 'length' expects exactly one list argument. " ++ show arg

evalReverse :: Env -> [Expr] -> Either String (Env, Expr)
evalReverse env [List xs] = Right (env, List (reverse xs))
evalReverse _ _ = Left "Error: 'reverse' expects exactly one list argument."

evalMember :: Env -> [Expr] -> Either String (Env, Expr)
evalMember env [el, List ls] = Right (env, SBool $ any (equalExprs el) ls)
evalMember _ _ = Left "Wrong arguments for operator 'member'"

evalFilterPredicate :: Env -> [String] -> Expr -> Expr -> Either String Bool
evalFilterPredicate env params body el = do
    (_, res) <- eval env (List [Lambda params body, el])
    case res of
        SBool b -> return b
        _ -> Left "Predicate for operator 'filter' must return a boolean."

evalMapLambda :: Env -> [String] -> Expr -> Expr -> Either String Expr
evalMapLambda env params body el = do
    (_, res) <- eval env (List [Lambda params body, el])
    Right res

equalExprs :: Expr -> Expr -> Bool
equalExprs (Number x) (Number y) = x == y
equalExprs (SBool x) (SBool y) = x == y
equalExprs (Str x) (Str y) = x == y
equalExprs (Symbol x) (Symbol y) = x == y
equalExprs (List xs) (List ys) = length xs == length ys && all (\(a, b) -> equalExprs a b) (zip xs ys)
equalExprs Null Null = True
equalExprs _ _ = False

bindParams :: [String] -> [Expr] -> Env ->  Either String Env
bindParams [] [] env = Right env
bindParams (param : params) (value : values) env = do
    bindParams params values ((param, value) : env)
bindParams _ _ _ = Left "Mismatch between parameters and arguments"

extractSymbol :: Expr -> Either String String
extractSymbol (Symbol s) = Right s
extractSymbol (Str s) = Right s
extractSymbol _ = Left "Function argument must be a symbol or string."


-- Repl helper
printEnv :: Env -> IO ()
printEnv env = do
    putStrLn "Current Environment:"
    mapM_ (\(var, expr) -> putStrLn $ var ++ " = " ++ show expr) env


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


-- Parser

parseInput :: String -> Either String Expr
parseInput input =
    case parseTokens (tokenize input) of
        Right (expr, []) -> Right expr
        Right (_, remaining) -> Left $ "Unexpected tokens: " ++ unwords remaining
        Left err -> Left err

parseTokens :: [String] -> Either String (Expr, [String])
parseTokens [] = Left "Unexpected end of input"
parseTokens ("(":ts) = do
    (insideExprs, remaining) <- parseList ts
    Right (List insideExprs, remaining)
parseTokens ("[":ts) = do
    (insideExprs, remaining) <- parseList ts
    Right (List insideExprs, remaining)
parseTokens ("'":ts) = do
    (insideExprs, remaining) <- parseTokens ts
    Right (List [Symbol "'", insideExprs], remaining)
parseTokens (")":_) = Left "Unmatched closing parenthesis"
parseTokens ("]":_) = Left "Unmatched closing bracket"
parseTokens ("#t":ts) = Right (SBool True, ts)
parseTokens ("#f":ts) = Right (SBool False, ts)
parseTokens (('"':str):ts)
    | last str == '"' = Right (Str (init str), ts)
    | otherwise = Left "Unmatched double quote in string"
parseTokens (t:ts) =
    case readMaybe t of
        Just n -> Right (Number n, ts)
        Nothing -> Right (Symbol t, ts)

parseList :: [String] -> Either String ([Expr], [String])
parseList [] = Left "Unmatched opening parenthesis"
parseList (")":ts) = Right ([], ts)
parseList ("]":ts) = Right ([], ts)
parseList ts = do
    (expr, rest) <- parseTokens ts
    (restList, remaining) <- parseList rest
    Right (expr : restList, remaining)

tokenize :: String -> [String]
tokenize [] = []
tokenize ('"':cs) =
    let (str, rest) = span (/= '"') cs
    in ('"' : str ++ "\"") : tokenize (drop 1 rest)
tokenize (c:cs)
    | c == '(' || c == ')' = [c] : tokenize cs
    | c == '[' || c == ']' = [c] : tokenize cs
    | c == '\'' = "'" : tokenize cs
    | c == ' ' = tokenize cs
    | otherwise =
        let (tok, rest) = span (`notElem` " ()") (c:cs)
        in tok : tokenize rest
