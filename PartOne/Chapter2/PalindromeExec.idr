module Main

import Palindrome

main : IO ()
main = repl "Enter a palindrome: " (\str => (show $ palindrome 0 str) ++ "\n")
