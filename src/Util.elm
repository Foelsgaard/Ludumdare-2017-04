module Util exposing (..)

import Model exposing (..)

import Time exposing (Time)

origin = {x = 0, y = 0}

gravity : Vector -> Planet -> Vector
gravity p planet =
    let acc = (g * planet.mass / ((dist (planetPos planet) p)^2))
    in normalize (planetPos planet .- p) |> scale acc

dist : Vector -> Vector -> Float
dist v1 v2 =
    sqrt ((v1.x - v2.x)^2 + (v1.y - v2.y)^2)

normalize : Vector -> Vector
normalize v =
    { x = v.x / (dist origin v)
    , y = v.y / (dist origin v)
    }

planetPos : Planet -> Vector
planetPos planet =
    { x = cos planet.angle * planet.distToStar
    , y = sin planet.angle * planet.distToStar
    }

checkCollision : Vector -> Float -> Vector -> Float -> Bool
checkCollision p1 r1 p2 r2 =
    dist p1 p2 < r1 + r2

find p = List.filter p >> List.head

vAdd : Vector -> Vector -> Vector
vAdd v1 v2 = { x = v1.x + v2.x
             , y = v1.y + v2.y
             }

(.+) = vAdd

vSub : Vector -> Vector -> Vector
vSub v1 v2 = { x = v1.x - v2.x
             , y = v1.y - v2.y
             }

(.-) = vSub

scale : Float -> Vector -> Vector
scale scalar v =
    { x = scalar * v.x
    , y = scalar * v.y
    }

vSum : List Vector -> Vector
vSum = List.foldr vAdd origin

vClamp : Float -> Float -> Vector -> Vector
vClamp low high {x, y} =
    { x = clamp low high x
    , y = clamp low high y
    }
