import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Nat)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

TupleVect : (length : Nat) -> (type : Type) -> Type
TupleVect Z type = ()
TupleVect (S k) type = (type, TupleVect k type)

test : TupleVect 4 Nat
test = (1,2,3,4,())
