module Main

-- Enumerated Types
data Direction = North
               | East
               | South
               | West

turnClockWise : Direction -> Direction
turnClockWise North = East
turnClockWise East = South
turnClockWise South = West
turnClockWise West = North

-- Union types
||| Represents shapes
data Shape = ||| A Triangle, with its base and length type
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle with its radius
             Circle Double


area : Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic1 pic2) = pictureArea pic1 + pictureArea pic2
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

trianglesInPicture : Picture -> List Shape
trianglesInPicture (Primitive (Triangle x y)) = [Triangle x y]
trianglesInPicture (Primitive (Rectangle x y)) = []
trianglesInPicture (Primitive (Circle x)) = []
trianglesInPicture (Combine x y) = trianglesInPicture x ++ trianglesInPicture y
trianglesInPicture (Rotate x y) = trianglesInPicture y
trianglesInPicture (Translate x y z) = trianglesInPicture z

getBiggest : List Shape -> Shape -> Biggest
getBiggest [] x = Size (area x)
getBiggest (y :: xs) x = if (area y > area x)
                         then getBiggest xs y
                         else getBiggest xs x

biggestTriangle : Picture -> Biggest
biggestTriangle x = case (trianglesInPicture x) of
                          [] => NoTriangle
                          x :: xs => getBiggest xs x
