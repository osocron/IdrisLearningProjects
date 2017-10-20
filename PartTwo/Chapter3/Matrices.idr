module Matrices

import Data.Vect

map2Vect : (f: a -> a -> b) ->
           Vect n a ->
           Vect n a ->
           Vect n b
map2Vect f [] [] = []
map2Vect f (y :: xs) (z :: ys) = (f y z) :: map2Vect f xs ys

addMatrix : Num numType =>
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (map2Vect (\x, y => x + y) x y) :: addMatrix xs ys

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) ->
                  (xsTrans : Vect n (Vect len elem)) ->
                  Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) ->
               Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         zipWith (::) x xsTrans

multiplyOne : Num numType =>
              Vect n numType ->
              Vect n numType ->
              numType
multiplyOne xs ys = sum (zipWith (*) xs ys)


multMany : Num numType =>
           Vect n numType ->
           Vect m (Vect n numType) ->
           Vect m numType
multMany xs ys = map (\y => multiplyOne xs y) ys

multMatrix : Num numType =>
             Vect n (Vect m numType) ->
             Vect m (Vect p numType) ->
             Vect n (Vect p numType)
multMatrix xs ys = let transposed = transposeMat ys in
                       map (\x => multMany x transposed) xs
