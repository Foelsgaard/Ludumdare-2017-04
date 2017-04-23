module Vector exposing (..)

type alias Vector = (Float, Float)
type alias Point = Vector

add : Vector -> Vector -> Vector
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(.+) = add

sub : Vector -> Vector -> Vector
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

(.-) = sub

scale : Float -> Vector -> Vector
scale scalar (x, y) = (x * scalar, y * scalar)

sum : List Vector -> Vector
sum = List.foldr add (0, 0)

clamp : Float -> Float -> Vector -> Vector
clamp low high (x, y) =
    (Basics.clamp low high x, Basics.clamp low high y)

norm : Vector -> Float
norm (x, y) = sqrt (x^2 + y^2)

dist : Point -> Point -> Float
dist p1 p2 = norm (p1 .- p2)

normalize : Vector -> Vector
normalize (x, y) = scale (1 / norm (x, y)) (x, y)

rotate : Float -> Vector -> Vector
rotate r (x, y) =
    ( x * cos r - y * sin r
    , x * sin r + y * cos r
    )
