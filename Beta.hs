
module Beta where

import Prelude
import Types
import Terms
import TypeCheck
import Subst
import GHC.Stack

beta :: Term -> Term
beta e =
    case e of
        Fst (Pair e1 e2)         -> e1
        Snd (Pair e1 e2)         -> e2
        App (Lambda y t1 e1) e2  -> subst e1 y e2
        App e1 e2                -> App (beta e1) (beta e2)
        Lambda y t1 e1           -> Lambda y t1 (beta e1)
        Plus e1 e2               -> let ee1 = beta e1
                                        ee2 = beta e2
                                     in case (e1, e2) of
                                         (ConstI n, ConstI m) -> ConstI (n + m)
                                         (_,_)                -> Plus ee1 ee2 
        Mult e1 e2               -> let ee1 = beta e1
                                        ee2 = beta e2
                                     in case (e1, e2) of
                                         (ConstI n, ConstI m) -> ConstI (n * m)
                                         (_,_)                -> Mult ee1 ee2 
        Minus e1 e2              -> let ee1 = beta e1
                                        ee2 = beta e2
                                     in case (e1, e2) of
                                         (ConstI n, ConstI m) -> ConstI (n - m)
                                         (_,_)                -> Minus ee1 ee2 
        IsEq e1 e2               -> let ee1 = beta e1
                                        ee2 = beta e2
                                    in case (e1, e2) of
                                        (ConstI n, ConstI m)     -> ConstB (n == m)
                                        (ConstB n, ConstB m)     -> ConstB (n == m)
                                        (Cons e1 e2, Cons e3 e4) -> if e1 == e3 then IsEq e2 e4 else ConstB False
                                        (Nil t1, Nil t2)         -> ConstB (t1 == t2)
                                        (Nil t1, _)              -> ConstB (False)
                                        (_, Nil t2)              -> ConstB (False)
                                        (_,_)                    -> IsEq ee1 ee2
        Ite (ConstB True) e1 e2  -> e1
        Ite (ConstB False) e1 e2 -> e2 
        Ite e1 e2 e3             -> Ite (beta e1) e2 e3
        Fix (Lambda y t1 e1)     -> subst e1 y (Fix (Lambda y t1 e1))
        Fix e1                   -> let ee1 = beta e1
                                    in Fix ee1
        TApp (PLambda y e1) t1   -> substTermType e1 y t1
        TApp e1 t1               -> let ee1 = beta e1
                                    in TApp ee1 t1
  --      Fix (TApp (PLambda y e1) t1) -> Fix (substTermType e1 y t1)
        Head (Cons e1 e2)        -> e1
        Tail (Cons e1 e2)        -> e2
        Cons e1 e2               -> Cons (beta e1) (beta e2)   
        _                        -> e

--[fix ((Λa. (λf: ([a] -> Z). (λx: [a]. if (x == (nil: a)) then { 0 } else { (1 + [f tl x]) })))[Z] (5 :: (nil: Z))]

--fix [(Λa. (λf: ([a] -> Z). (λx: [a]. if (x == (nil: a)) then { 0 } else { (1 + [f tl x]) }))) (5 :: (nil: Z))][Z]

typed_beta :: Ctx -> Term -> Term
typed_beta m e = 
    case (typeCheck m e) of
        Yes _ -> beta e
        No s  -> error s

refl_trans_beta :: Term -> Term 
refl_trans_beta e = let e' = beta e in if e == e' then e else refl_trans_beta e'

refl_trans_typed_beta :: Ctx -> Term -> Term
refl_trans_typed_beta m e =
    case (typeCheck m e) of
        Yes _ -> refl_trans_beta e
        No s  -> error s