lastElem : List a -> a
lastElem xs = case xs of
                   last :: [] => last
                   h :: t => lastElem t
