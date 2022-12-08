
module ListOps where

import Prelude
import Types
import Terms
import TypeCheck
import Subst
import Beta
import Fibonacci
import Factorial

len :: Term
len =
    PLambda "a"
    (
       Lambda "f"
              (Arrow (List (TVar "a")) TInt)
              (
                     Lambda "x"
                            (List (TVar "a"))
                            (
                                   Ite
                                   (IsEq (Var "x") (Nil (TVar "a")))
                                   (ConstI 0)
                                   (Plus (ConstI 1)(App(Var "f")(Tail (Var "x"))))
                            )
              )
    )

length :: Term -> Type -> Term
length l t = App (Fix (TApp len t)) l

appH :: Term
appH =
       PLambda "a"
       (
              Lambda "f"
                     (Arrow (List (TVar "a")) (Arrow (List (TVar "a")) (List (TVar "a"))))
                     (
                            Lambda "x"
                                   (List (TVar "a"))
                                   (
                                          Lambda "y"
                                                 (List (TVar "a"))
                                                 (
                                                        Ite
                                                        (IsEq (Var "x")(Nil (TVar "a")))
                                                        (Var "y")
                                                        (Cons (Head (Var "x"))(App(App(Var "f")(Tail (Var "x")))(Var "y")))
                                                 )
                                   )
                     )
       )

append :: Term -> Term -> Type -> Term
append l1 l2 t = App(App(Fix (TApp appH t))(l1))(l2)