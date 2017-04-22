module Util exposing (..)

import Model exposing (..)

import Time exposing (Time)

gravity : Vector -> Planet -> Vector
gravity p planet =
    let acc = (g * planet.mass / ((dist (planetPos planet) p)^2))
    in normalize (planetPos planet .- p) |> vScale acc

dist : Vector -> Vector -> Float
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

normalize : Vector -> Vector
normalize (x, y) =
    let d = dist (0,0) (x, y)
    in vScale (1 / d) (x, y)

planetPos : Planet -> Vector
planetPos planet =
    (cos planet.orbitalAngle, sin planet.orbitalAngle)
        |> vScale planet.orbitalRadius

checkCollision : Vector -> Float -> Vector -> Float -> Bool
checkCollision p1 r1 p2 r2 =
    dist p1 p2 < r1 + r2

find p = List.filter p >> List.head

vAdd : Vector -> Vector -> Vector
vAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(.+) = vAdd

vSub : Vector -> Vector -> Vector
vSub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

(.-) = vSub

vScale : Float -> Vector -> Vector
vScale scalar (x, y) = (x * scalar, y * scalar)

vSum : List Vector -> Vector
vSum = List.foldr vAdd (0, 0)

vClamp : Float -> Float -> Vector -> Vector
vClamp low high (x, y) = (clamp low high x, clamp low high y)
