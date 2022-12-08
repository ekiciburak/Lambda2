
module Factorial where

import Prelude
import Types
import Terms
import TypeCheck
import Subst
import Beta

fact :: Term
fact =
    Lambda "f"
           (Arrow TInt TInt)
           (
            Lambda "x"
                   (TInt)
                   (
                    Ite
                    (IsEq (Var "x") (ConstI 0))
                    (ConstI 1)
                    (Mult (Var "x")(App(Var "f")(Minus (Var "x")(ConstI 1))))
                   )
           )

factorial :: Term -> Term
factorial n = App(Fix fact)(n)