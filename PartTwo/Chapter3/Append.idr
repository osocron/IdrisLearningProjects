module Main

import Data.Vect

append : (elem: Type) -> (n: Nat) -> (m: Nat) ->
        Vect n elem -> Vect m elem ->
        Vect (n + m) elem
append elem Z m [] ys = ys
append elem (S len) m (x :: xs) ys = x :: append elem len m xs ys

length : Vect n elem -> Nat
length {n} xs = n
