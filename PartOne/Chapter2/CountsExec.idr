module Main

import Palindrome

main : IO ()
main = repl "Let's count your words: " (\str => (show $ counts str) ++ "\n")
