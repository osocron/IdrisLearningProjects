module Main

describeList : List Int -> String
describeList [] = "Empty"
describeList (x :: xs) = "Non empty, tail = " ++ show xs

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs
