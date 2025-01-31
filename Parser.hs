module Parser where

import Text.Read (readMaybe)
import Expr

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
