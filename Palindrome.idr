module Palindrome

||| Checks if the given string is a palindrome and has at least n length.
palindrome : Nat -> String -> Bool
palindrome n str = length str > n &&
                   reverse (toLower str) == toLower str

||| Counts the number of words in a string and the length of the string.
counts :  String -> (Nat, Nat)
counts str = (length $ words str, length str)

||| Returns the 10 greatest items on a List that can be ordered.
top_ten : Ord a => List a -> List a
top_ten xs = take 10 $ reverse $ sort xs

over_length : Nat -> List String -> Nat
over_length n xs = length (filter (\s => length s > n) xs)
