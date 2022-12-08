
module Subst where

import Prelude
import Types
import Terms

import Data.IORef
import System.IO.Unsafe

counterRef :: IORef Int
counterRef = unsafePerformIO (newIORef 0)

incrementCount :: IO Int
incrementCount = atomicModifyIORef (counterRef) (\c -> (c+1, c+1))

readCount :: IO Int
readCount = readIORef counterRef

resetCount :: IO Int
resetCount = atomicModifyIORef (counterRef) (\c -> (0, 0))

freshVar :: String -> String
freshVar y = 
    let x = unsafePerformIO incrementCount in y ++ (show x)

find :: Eq a => a -> [a] -> Bool
find y l =
    case l of
        []   -> False
        x:xs -> if x == y then True else find y xs

uniqueH :: Eq a => [a] -> [a] -> [a]
uniqueH l acc = 
    case l of
        []   -> acc
        x:xs -> if find x acc then uniqueH xs acc else uniqueH xs (acc ++ [x])

unique :: Eq a => [a] -> [a]
unique l = uniqueH l []

fvH :: Term -> [String] -> [String]
fvH e acc =
    case e of
        Pair e1 e2     -> unique (fvH e1 acc ++ fvH e2 acc)
        Fst e1         -> unique (fvH e1 acc)
        Snd e1         -> unique (fvH e1 acc)
        Var s          -> s : acc
        Lambda y t1 e1 -> filter (\a -> a /= y) (fvH e1 acc)
        App e1 e2      -> unique (fvH e1 acc ++ fvH e2 acc)
        Plus e1 e2     -> unique (fvH e1 acc ++ fvH e2 acc)
        Mult e1 e2     -> unique (fvH e1 acc ++ fvH e2 acc)
        Minus e1 e2    -> unique (fvH e1 acc ++ fvH e2 acc)
        IsEq e1 e2     -> unique (fvH e1 acc ++ fvH e2 acc)
        Ite e1 e2 e3   -> unique (fvH e1 acc ++ fvH e2 acc ++ fvH e3 acc)
        Fix e1         -> unique (fvH e1 acc)
        PLambda y e1   -> filter (\a -> a /= y) (fvH e1 acc)
        TApp e1 t2     -> unique (fvH e1 acc)
        Cons e1 e2     -> unique (fvH e1 acc ++ fvH e2 acc)
        Head e1        -> unique (fvH e1 acc)
        Tail e1        -> unique (fvH e1 acc)
        _              -> acc


fv :: Term -> [String]
fv t = fvH t []

fvTH :: Type -> [String] -> [String]
fvTH t acc =
    case t of
        TVar s      -> s : acc
        Forall y t1 -> filter (\a -> a /= y) (fvTH t1 acc)
        Arrow t1 t2 -> unique (fvTH t1 acc ++ fvTH t2 acc)
        Prod t1 t2  -> unique (fvTH t1 acc ++ fvTH t2 acc)
        List t1     -> unique (fvTH t1 acc)
        _           -> acc

fvT :: Type -> [String]
fvT t = fvTH t []

inTerm :: String -> Term -> Bool
inTerm x e =
    case e of
        Pair e1 e2     -> inTerm x e1 || inTerm x e2
        Fst e1         -> inTerm x e1
        Snd e1         -> inTerm x e1
        Var s          -> x == s
        Lambda y t1 e1 -> if x == y then True else inTerm x e1
        App e1 e2      -> inTerm x e1 || inTerm x e2
        Plus e1 e2     -> inTerm x e1 || inTerm x e2
        Mult e1 e2     -> inTerm x e1 || inTerm x e2
        Minus e1 e2    -> inTerm x e1 || inTerm x e2
        IsEq e1 e2     -> inTerm x e1 || inTerm x e2
        Ite e1 e2 e3   -> inTerm x e1 || inTerm x e2 || inTerm x e3
        Fix e1         -> inTerm x e1
        PLambda y e1   -> if x == y then True else inTerm x e1
        TApp e1 t1     -> inTerm x e1
        Cons e1 e2     -> inTerm x e1 || inTerm x e2
        Head e1        -> inTerm x e1
        Tail e1        -> inTerm x e1
        _              -> False

inType :: String -> Type -> Bool
inType x t =
    case t of
        TVar s      -> x == s
        Forall y t1 -> if x == y then True else inType x t1
        Arrow t1 t2 -> inType x t1 || inType x t2
        Prod t1 t2  -> inType x t1 || inType x t2 
        List t1     -> inType x t1
        _           -> False

replace :: String -> String -> Term -> Term
replace x y e =
    case e of
        Pair e1 e2     -> Pair (replace x y e1) (replace x y e2)
        Fst e1         -> Fst (replace x y e1)
        Snd e1         -> Snd (replace x y e1)
        Var s          -> if x == s then Var y else e
        Lambda s t1 e1 -> if x == s then Lambda y t1 (replace x y e1) else Lambda s t1 (replace x y e1)
        App e1 e2      -> App (replace x y e1) (replace x y e2) 
        Plus e1 e2     -> Plus (replace x y e1) (replace x y e2) 
        Mult e1 e2     -> Mult (replace x y e1) (replace x y e2) 
        Minus e1 e2    -> Minus (replace x y e1) (replace x y e2) 
        IsEq e1 e2     -> IsEq (replace x y e1) (replace x y e2) 
        Ite e1 e2 e3   -> Ite (replace x y e1) (replace x y e2) (replace x y e3)
        Fix e1         -> Fix (replace x y e1)
        PLambda s e1   -> if x == s then PLambda y (replace x y e1) else PLambda s (replace x y e1)
        TApp e1 t1     -> TApp (replace x y e1) t1
        Cons e1 e2     -> Cons (replace x y e1) (replace x y e2)
        Head e1        -> Head (replace x y e1)
        Tail e1        -> Tail (replace x y e1)
        _              -> e

replaceType :: String -> String -> Type -> Type
replaceType x y t =
    case t of
        TVar s      -> if x == s then TVar y else t
        Forall s t1 -> if x == s then Forall y (replaceType x y t1) else Forall s (replaceType x y t1)
        Arrow t1 t2 -> Arrow (replaceType x y t1) (replaceType x y t2)
        Prod t1 t2  -> Prod (replaceType x y t1) (replaceType x y t2)
        List t1     -> List (replaceType x y t1)
        _           -> t


alpha :: String -> Term -> Term
alpha x e =
    case e of
        Pair e1 e2     -> Pair (alpha x e1) (alpha x e2)
        Fst e1         -> Fst (alpha x e1)
        Snd e1         -> Snd (alpha x e1)
        Var y          -> Var y
        Lambda y t1 e1 -> if y /= x && inTerm x e1 == False then Lambda x t1 (replace y x e1) else e
        App e1 e2      -> App (alpha x e1) (alpha x e2)
        Plus e1 e2     -> Plus (alpha x e1) (alpha x e2)
        Mult e1 e2     -> Mult (alpha x e1) (alpha x e2)
        Minus e1 e2    -> Minus (alpha x e1) (alpha x e2)
        IsEq e1 e2     -> IsEq (alpha x e1) (alpha x e2)
        Ite e1 e2 e3   -> Ite (alpha x e1) (alpha x e2) (alpha x e3)
        Fix e1         -> Fix (alpha x e1)
        PLambda y e1   -> if y /= x && inTerm x e1 == False then PLambda x (replace y x e1) else e
        TApp e1 t1     -> TApp (alpha x e1) t1
        Cons e1 e2     -> Cons (alpha x e1) (alpha x e2)
        Head e1        -> Head (alpha x e1)
        Tail e1        -> Tail (alpha x e1)
        _              -> e

alphaType :: String -> Type -> Type
alphaType x t =
    case t of
        Forall y t1 -> if y /= x && inType x t1 == False then Forall x (replaceType y x t1) else t
        Arrow t1 t2 -> Arrow (alphaType x t1) (alphaType x t2)
        Prod t1 t2  -> Prod (alphaType x t1) (alphaType x t2)
        List t1     -> List (alphaType x t1)
        _           -> t


subst :: Term -> String -> Term -> Term
subst e x s =
    case e of
        Pair e1 e2      -> Pair (subst e1 x s) (subst e2 x s)
        Fst e1          -> Fst (subst e1 x s)
        Snd e1          -> Snd (subst e1 x s)
        Var y           -> if x == y then s else e
        Lambda y t1 e1  -> if x /= y && find y (fv s) == False 
                           then Lambda y t1 (subst e1 x s)
                           else if x == y && find y (fv s) == False 
                           then e
                           else let z  = freshVar y
                                    e' = alpha z e
                                in subst e' x s
        App e1 e2    -> App (subst e1 x s) (subst e2 x s)
        Plus e1 e2   -> Plus (subst e1 x s) (subst e2 x s)
        Mult e1 e2   -> Mult (subst e1 x s) (subst e2 x s)
        Minus e1 e2  -> Minus (subst e1 x s) (subst e2 x s)
        IsEq e1 e2   -> IsEq (subst e1 x s) (subst e2 x s)
        Ite e1 e2 e3 -> Ite (subst e1 x s) (subst e2 x s) (subst e3 x s)
        Fix e1       -> Fix (subst e1 x s)
        PLambda y e1 -> if x /= y && find y (fv s) == False 
                        then PLambda y (subst e1 x s)
                        else if x == y && find y (fv s) == False 
                        then e
                        else let z  = freshVar y
                                 e' = alpha z e
                             in subst e' x s
        TApp e1 t1   -> TApp (subst e1 x s) t1
        Cons e1 e2   -> Cons (subst e1 x s) (subst e2 x s)
        Head e1      -> Head (subst e1 x s)
        Tail e1      -> Tail (subst e1 x s)
        _            -> e

substType :: Type -> String -> Type -> Type
substType t x s =
    case t of
        Forall y t1 -> if x /= y && find y (fvT s) == False 
                       then Forall y (substType t1 x s)
                       else if x == y && find y (fvT s) == False 
                       then t
                       else let z  = freshVar y
                                t' = alphaType z t
                            in substType t' x s
        Arrow t1 t2 -> Arrow (substType t1 x s) (substType t2 x s)
        Prod t1 t2  -> Prod (substType t1 x s) (substType t2 x s)
        TVar y      -> if x == y then s else t 
        List t1     -> List (substType t1 x s)
        _           -> t

substTermType :: Term -> String -> Type -> Term
substTermType e x t =
    case e of
        Pair e1 e2     -> Pair (substTermType e1 x t) (substTermType e2 x t)
        Fst e1         -> Fst (substTermType e1 x t)
        Snd e1         -> Snd (substTermType e1 x t)
        Lambda y t1 e1 -> Lambda y (substType t1 x t) (substTermType e1 x t)
        App e1 e2      -> App (substTermType e1 x t) (substTermType e2 x t)
        TApp e1 t1     -> TApp (substTermType e1 x t) (substType t1 x t)
        PLambda y e1   -> PLambda y (substTermType e1 x t) 
        Plus e1 e2     -> Plus (substTermType e1 x t) (substTermType e2 x t)
        Mult e1 e2     -> Mult (substTermType e1 x t) (substTermType e2 x t) 
        Minus e1 e2    -> Minus (substTermType e1 x t) (substTermType e2 x t) 
        IsEq e1 e2     -> IsEq (substTermType e1 x t) (substTermType e2 x t) 
        Ite e1 e2 e3   -> Ite (substTermType e1 x t) (substTermType e2 x t) (substTermType e3 x t)
        Fix e1         -> Fix (substTermType e1 x t) 
        Cons e1 e2     -> Cons (substTermType e1 x t) (substTermType e2 x t)
        Head e1        -> Head (substTermType e1 x t)
        Tail e1        -> Tail (substTermType e1 x t)
        Nil t1         -> Nil (substType t1 x t)
        _              -> e


termEq :: Term -> Term -> Bool
termEq e1 e2 =
    case (e1, e2) of
        (ConstI n, ConstI m)               -> n == m
        (ConstB n, ConstB m)               -> n == m
        (Tt, Tt)                           -> True
        (Pair e1 e2, Pair e3 e4)           -> termEq e1 e3 && termEq e2 e4
        (Fst e1, Fst e2)                   -> termEq e1 e2
        (Snd e1, Snd e2)                   -> termEq e1 e2
        (Var s1, Var s2)                   -> s1 == s2
        (Lambda y1 t1 e1, Lambda y2 t2 e2) -> let fv  = freshVar "x"
                                                  e1' = subst e1 y1 (Var fv)
                                                  e2' = subst e2 y2 (Var fv)
                                              in termEq e1' e2' && typeEq t1 t2
        (App e1 e2, App e3 e4)             -> termEq e1 e3 && termEq e2 e4
        (Plus e1 e2, Plus e3 e4)           -> termEq e1 e3 && termEq e2 e4
        (Mult e1 e2, Mult e3 e4)           -> termEq e1 e3 && termEq e2 e4
        (Minus e1 e2, Minus e3 e4)         -> termEq e1 e3 && termEq e2 e4
        (IsEq e1 e2, IsEq e3 e4)           -> termEq e1 e3 && termEq e2 e4
        (Ite e1 e2 e3, Ite e4 e5 e6)       -> termEq e1 e4 && termEq e2 e5 && termEq e3 e6
        (Fix e1, Fix e2)                   -> termEq e1 e2
        (PLambda y1 e1, PLambda y2 e2)     -> let fv  = freshVar "x"
                                                  e1' = subst e1 y1 (Var fv)
                                                  e2' = subst e2 y2 (Var fv)                                                
                                              in termEq e1' e2
        (TApp e1 t1, TApp e2 t2)           -> termEq e1 e2 && typeEq t1 t2
        (Cons e1 e2, Cons e3 e4)           -> termEq e1 e3 && termEq e2 e4
        (Head e1, Head e2)                 -> termEq e1 e2
        (Tail e1, Tail e2)                 -> termEq e1 e2
        (Nil t1, Nil t2)                   -> typeEq t1 t2
        (_, _)                             -> False

instance Eq Term where
    t1 == t2 = termEq t1 t2

typeEq :: Type -> Type -> Bool
typeEq t1 t2 =
    case (t1, t2) of
        (TInt, TInt)                 -> True
        (TBool, TBool)               -> True
        (TUnit, TUnit)               -> True
        (TVar s1, TVar s2)           -> s1 == s2
        (Arrow t1 t2, Arrow t3 t4)   -> typeEq t1 t3 && typeEq t2 t4
        (Prod t1 t2, Prod t3 t4)     -> typeEq t1 t3 && typeEq t2 t4
        (Forall y1 t1, Forall y2 t2) -> let fv  = freshVar "x"
                                            t1' = substType t1 y1 (TVar fv)
                                            t2' = substType t2 y2 (TVar fv)                                                
                                        in typeEq t1' t2'
        (List t1, List t2)           -> typeEq t1 t2
        (_,_)                        -> False

instance Eq Type where
    t1 == t2 = typeEq t1 t2