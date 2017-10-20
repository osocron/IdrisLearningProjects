module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

countdown : (secs: Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do putStrLn $ show $ S secs
                        usleep 100000
                        countdown secs

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do putStr "Guess the number: "
                          Just number <- readNumber
                               | Nothing => do putStrLn "Invalid input!"
                                               guess target guesses
                          if number == target
                             then putStrLn $ "You won after " ++ (show guesses) ++ " times!"
                             else if number > target
                                     then do putStrLn "Too high!"
                                             guess target (guesses + 1)
                                     else do putStrLn "Too low!"
                                             guess target (guesses + 1)

repl' : (prompt : String) -> (onInput : String -> String) -> IO ()
repl' prompt onInput = do putStr prompt
                          input <- getLine
                          putStrLn $ onInput input
                          repl' prompt onInput

replWith' : (state : a) ->
            (prompt : String) ->
            (onInput : a -> String -> Maybe (String, a)) -> IO ()
replWith' state prompt onInput = do putStr prompt
                                    input <- getLine
                                    case onInput state input of
                                         Just (result, newState) => do putStr result
                                                                       replWith' newState prompt onInput
                                         Nothing => do pure ()

main : IO ()
main = do num <- time
          guess (fromIntegerNat num) 0
