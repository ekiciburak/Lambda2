{-#LANGUAGE GADTs#-}

module TypeCheck where

import Prelude
import Types
import Terms
import Subst

data Err a where
    No  :: String -> Err a
    Yes :: a      -> Err a

errEq :: Eq a => Err a -> Err a -> Bool
errEq e1 e2 =
    case (e1, e2) of
        (Yes n1, Yes n2) -> n1 == n2
        (_,_)            -> False

instance Eq a => Eq (Err a) where
    e1 == e2 = errEq e1 e2

err2String :: Show a => Err a -> String
err2String e =
    case e of
        Yes x -> show x
        No s  -> s

instance Show a => Show (Err a) where
    show e = err2String e

type Ctx = [(String, Type)]

extend :: Ctx -> String -> Type -> Ctx
extend c s t = (s, t) : c

lookup :: Ctx -> String -> Err Type
lookup c s =
    case c of
        []       -> No "no such term exists in the context"
        (n,t):xs -> if n == s then Yes t else TypeCheck.lookup xs s

typeCheck :: Ctx -> Term -> Err Type
typeCheck m e =
    case e of
        ConstI n       -> Yes TInt
        ConstB b       -> Yes TBool
        Tt             -> Yes TUnit
        Pair e1 e2     -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                             (Yes yte1, Yes yte2) -> Yes (Prod yte1 yte2)
                             (_,_)                -> No "typing error in pair"
        Fst e1         -> let te1 = typeCheck m e1
                          in case te1 of
                                Yes (Prod f s) -> Yes f
                                _              -> No "typing error in fst"
        Snd e1         -> let te1 = typeCheck m e1
                          in case te1 of
                                Yes (Prod f s) -> Yes s
                                _              -> No "typing error in snd"
        Var s          -> TypeCheck.lookup m s
        Lambda y t1 e1 -> let m'  = extend m y t1
                              te1 = typeCheck m' e1
                          in case te1 of
                                Yes yte1 -> Yes (Arrow t1 yte1)
                                _        -> No "typing error in lambda"
        App e1 e2      -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                 (Yes (Arrow s t), Yes yte2) | s == yte2 -> Yes t
                                 _                                       -> No "typing error in app"
        Plus e1 e2     -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                 (Yes TInt, Yes TInt) -> Yes TInt
                                 (_,_)                -> No "typing error in plus"
        Mult e1 e2     -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                 (Yes TInt, Yes TInt) -> Yes TInt
                                 (_,_)                -> No "typing error in mult"
        Minus e1 e2    -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                 (Yes TInt, Yes TInt) -> Yes TInt
                                 (_,_)                -> No "typing error in minus"
        IsEq e1 e2     -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in if te1 == te2 then Yes TBool else No "typing error in iseq"
        Ite e1 e2 e3   -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                              te3 = typeCheck m e3
                          in if te1 == Yes TBool && te2 == te3 then te2 else No "typing error in ite"
        Fix e1         -> let te1 = typeCheck m e1
                          in case te1 of
                                 Yes (Arrow s t) | s == t -> Yes t
                                 _                        -> No "typing error in fix"
        PLambda y e1   -> let m'  = extend m y (TVar y)
                              te1 = typeCheck m' e1
                          in case te1 of
                                Yes yte1 -> Yes (Forall y yte1)
                                _        -> No "typing error in plambda"
        TApp e1 t2     -> let te1 = typeCheck m e1
                          in case te1 of
                                Yes (Forall x t1) -> Yes (substType t1 x t2)
                                _                 -> No "typing error in tapp"
        Cons e1 e2     -> let te1 = typeCheck m e1
                              te2 = typeCheck m e2
                          in case (te1, te2) of
                                (Yes yte1, Yes (List yte2)) | yte1 == yte2 -> Yes (List yte1)
                                (_, _)                                     -> No "typing error in list cons"
        Nil t1         -> Yes (List t1)
        Head e1        -> let te1 = typeCheck m e1
                          in case te1 of
                                Yes (List yte1) -> Yes yte1
                                _               -> No "typing error in list head"
        Tail e1        -> let te1 = typeCheck m e1
                          in case te1 of
                                Yes (List yte1) -> Yes (List yte1)
                                _               -> No "typing error in list tail"

