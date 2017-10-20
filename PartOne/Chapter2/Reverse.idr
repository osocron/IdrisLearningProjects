module Main

main : IO ()
main = repl "> " reverse

myReverse : String -> String
myReverse str = if length str == 0 then ""
                else myReverse (strTail str) ++ singleton (strHead str)
