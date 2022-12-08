
module Fibonacci where

import Prelude
import Types
import Terms
import TypeCheck
import Subst
import Beta


fib :: Term
fib =
    Lambda "f"
           (Arrow TInt TInt)
           (
            Lambda "x"
                   (TInt)
                   (
                    Ite
                    (IsEq (Var "x") (ConstI 0))
                    (ConstI 0)
                    (
                        Ite
                        (IsEq (Var "x") (ConstI 1))
                        (ConstI 1)
                        (Plus(App(Var "f")(Minus (Var "x")(ConstI 1)))(App(Var "f")(Minus (Var "x")(ConstI 2))))
                    )
                   )
           )

fibonacci :: Term -> Term
fibonacci e = App (Fix fib) e
