
{-# LANGUAGE GADTs #-}

module Types where

import Prelude 

data Type where
    TInt   :: Type
    TBool  :: Type
    TUnit  :: Type
    TVar   :: String -> Type
    Arrow  :: Type   -> Type -> Type
    Prod   :: Type   -> Type -> Type
    Forall :: String -> Type -> Type
    List   :: Type   -> Type

type2String :: Type -> String
type2String t =
    case t of 
        TInt        -> "Z"
        TBool       -> "B"
        TUnit       -> "1"
        TVar s      -> s
        Arrow t1 t2 -> "(" ++ type2String t1 ++ " -> " ++ type2String t2 ++ ")"
        Prod t1 t2  -> "(" ++ type2String t1 ++ " * "  ++ type2String t2 ++ ")"
        Forall s t1 -> "(∀" ++ s ++ ". " ++ type2String t1 ++ ")"
        List t1     -> "[" ++ type2String t1 ++ "]"


instance Show Type where
    show t = type2String t

