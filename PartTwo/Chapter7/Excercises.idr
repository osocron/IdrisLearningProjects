module Main

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (Triangle x y) == (Triangle z w) = x == z && y == w
  (Triangle x y) == that = False
  (Rectangle x y) == (Rectangle z w) = x == z && y == w
  (Rectangle x y) == that = False
  (Circle x) == (Circle y) = x == y
  (Circle x) == that = False

shapeArea : Shape -> Double
shapeArea (Triangle x y) = (x * y) / 2
shapeArea (Rectangle x y) = x * y
shapeArea (Circle x) = pi * (x * x)

Ord Shape where
  compare a b = compare (shapeArea a) (shapeArea b)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4,
              Rectangle 2 7]
