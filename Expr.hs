module Expr where

data Expr
    = Symbol String
    | Number Int
    | SBool Bool
    | Str String
    | List [Expr]
    | Lambda [String] Expr
    | Null
    deriving (Eq)

instance Show Expr where
    show :: Expr -> String
    show (Symbol s)     = s
    show (Number n)     = show n
    show (SBool b)      = if b then "#t" else "#f"
    show (Str s)        = "\"" ++ s ++ "\""
    show (List exprs)   = "'(" ++ unwords (map show exprs) ++ ")"
    show (Lambda args body) = "(lambda (" ++ unwords args ++ ") " ++ show body ++ ")"
    show Null           = ""
