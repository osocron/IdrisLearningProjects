data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Functor Tree where
  map f Empty = Empty
  map f (Node x y z) = Node (map f x) (f y) (map f z)

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node left e right)
      = let leftFold = foldr func acc left
            rightFold = foldr func leftFold right in
            func e rightFold
