module Main

import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x xs = case integerToFin x n of
                         Nothing => Nothing
                         (Just idx) => Just (index idx xs)

vectTake : (n : Fin m) -> Vect m a -> Vect (finToNat n) a
vectTake FZ (x :: xs) = []
vectTake (FS y) (x :: xs) = x :: vectTake y xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just x) => Just $ (index x xs) + (index x ys)
