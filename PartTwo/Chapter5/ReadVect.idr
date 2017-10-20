module Main

import Data.Vect

data VectUnknown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

readVect' : IO (VectUnknown String)
readVect' = do x <- getLine
               if (x == "")
                  then pure (MkVect _ [])
                  else do MkVect _ xs <- readVect'
                          pure $ MkVect _ (x :: xs)

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = putStrLn (show xs ++ " (length " ++ show len ++ ")")

anyVect : (n ** Vect n String)
anyVect = (3 ** ["Rod", "Jane", "Freddy"])

readVect : IO (len ** Vect len String)
readVect = do x <- getLine
              if (x == "")
                 then pure (_ ** [])
                 else do (_ ** xs) <- readVect
                         pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- readVect
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "Vectors are different lengths"
                    Just vec2' => printLn $ zip vec1 vec2'

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if x == ""
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)

readAndSave : IO ()
readAndSave = do input <- readToBlank
                 putStr "Write the file name: "
                 fileName <- getLine
                 let foldedInput = (foldl (\acc, str => acc ++ str ++ "\n") "" input)
                 do Right fileHandle <- fopen fileName "w"
                      | Left error => putStrLn "\nThere was an error opening the file"
                    Right _ <- fPutStr fileHandle foldedInput
                      | Left error => putStrLn "\nThere was an error writing to the file"
                    closeFile fileHandle
                    putStrLn "\nData written to file"

readLinesFromFile : (h: File) -> IO (n ** Vect n String)
readLinesFromFile h = do end <- fEOF h
                         if end
                            then pure (_ ** [])
                         else do Right line <- fGetLine h
                                   | Left _ => pure (_ ** [])
                                 (_ ** xs) <- readLinesFromFile h
                                 pure (_ ** line :: xs)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right fileHandle <- fopen filename "r"
                             | Left _ => pure (_ ** [])
                           result <- readLinesFromFile fileHandle
                           closeFile fileHandle
                           pure result

printVect' : (n ** Vect n String) -> IO ()
printVect' (n ** vect) = putStrLn (show vect)
