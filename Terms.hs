
{-# LANGUAGE GADTs #-}

module Terms where

import Prelude
import Types

data Term where
    ConstI  :: Int    -> Term
    ConstB  :: Bool   -> Term
    Tt      :: Term
    Pair    :: Term   -> Term -> Term
    Fst     :: Term   -> Term
    Snd     :: Term   -> Term
    Var     :: String -> Term
    Lambda  :: String -> Type -> Term -> Term
    App     :: Term   -> Term -> Term
    Plus    :: Term   -> Term -> Term
    Mult    :: Term   -> Term -> Term
    Minus   :: Term   -> Term -> Term
    IsEq    :: Term   -> Term -> Term
    Ite     :: Term   -> Term -> Term -> Term
    Fix     :: Term   -> Term
    PLambda :: String -> Term -> Term
    TApp    :: Term   -> Type -> Term
    Cons    :: Term   -> Term -> Term
    Nil     :: Type   -> Term
    Head    :: Term   -> Term
    Tail    :: Term   -> Term


term2String :: Term -> String
term2String e = 
    case e of
        ConstI n       -> show n
        ConstB b       -> show b
        Tt             -> "tt"
        Pair e1 e2     -> "(" ++ term2String e1 ++ "," ++ term2String e2 ++ ")"
        Fst e1         -> "fst(" ++ term2String e1 ++ ")"
        Snd e1         -> "snd(" ++ term2String e1 ++ ")"
        Var s          -> s
        Lambda y t1 e2 -> "(λ" ++ y ++ ": " ++ type2String t1 ++ ". " ++ term2String e2 ++ ")"
        App e1 e2      -> "[" ++ term2String e1 ++ " " ++ term2String e2 ++ "]"
        Plus e1 e2     -> "(" ++ term2String e1 ++ " + " ++ term2String e2 ++ ")"
        Mult e1 e2     -> "(" ++ term2String e1 ++ " x " ++ term2String e2 ++ ")"
        Minus e1 e2    -> "(" ++ term2String e1 ++ " - " ++ term2String e2 ++ ")"
        IsEq e1 e2     -> "(" ++ term2String e1 ++ " == " ++ term2String e2 ++ ")"
        Ite e1 e2 e3   -> "if " ++ term2String e1 ++ " then { " ++ term2String e2 ++ " } else { " ++ term2String e3 ++ " }"
        Fix e1         -> "fix " ++ term2String e1
        PLambda s e1   -> "(Λ" ++ s ++ ". " ++ term2String e1 ++ ")"
        TApp e1 t1     ->  term2String e1 ++ "[" ++ type2String t1 ++ "]"
        Cons e1 e2     -> "(" ++ term2String e1 ++ " :: " ++ term2String e2 ++ ")"
        Nil t1         -> "(nil: " ++ type2String t1 ++ ")"
        Head e1        -> "hd " ++ term2String e1 
        Tail e1        -> "tl " ++ term2String e1 


instance Show Term where  
    show e = term2String e