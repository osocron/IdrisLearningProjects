module Main

printLength : IO ()
printLength = getLine >>= \input => let len = length input in
                                        putStrLn (show len)

printLength' : IO ()
printLength' = do putStr "Input string: "
                  input <- getLine
                  let len = length input
                  putStrLn (show len)

printLonger : IO ()
printLonger = do putStr "First word: "
                 input1 <- getLine
                 let len1 = length input1
                 putStr "Second word: "
                 input2 <- getLine
                 let len2 = length input2
                 let longer = if len1 > len2 then len1 else len2
                 putStrLn (show longer)

printLonger' : IO ()
printLonger' = putStr "First word: " >>= \_ =>
               getLine >>= \input1 =>
               let len1 = length input1 in
               putStr "Second word: " >>= \_ =>
               getLine >>= \input2 =>
               let len2 = length input2
                   longer = if len1 > len2 then len1 else len2 in
               putStrLn (show longer)
