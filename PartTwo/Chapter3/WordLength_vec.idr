module Main

import Data.Vect

allLengths : Vect n String -> Vect n Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words
