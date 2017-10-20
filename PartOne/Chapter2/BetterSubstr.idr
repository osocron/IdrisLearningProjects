module Main

import Data.Vect

betterSubstr : (start: Nat) -> (howMany: Nat) -> Vect (start + howMany) Char -> String
betterSubstr start _ xs = foldl (\acc, elem => acc ++ (the String (cast elem))) "" (drop start xs)

main : IO ()
main = putStrLn $ show $ betterSubstr 2 3 (stringToVect "Hello")
  where
    stringToVect : (s: String) -> Vect (length $ unpack s) Char
    stringToVect str = fromList $ unpack str
