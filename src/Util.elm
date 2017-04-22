module Util exposing (..)

import Vector exposing (Vector, Point, (.-))

import Model exposing (..)

import Time exposing (Time)

gravity : Point -> Planet -> Vector
gravity p planet =
    let acc = (g * planet.mass / ((Vector.dist (planetPos planet) p)^2))
    in Vector.normalize (planetPos planet .- p) |> Vector.scale acc


planetPos : Planet -> Point
planetPos planet =
    (cos planet.orbitalAngle, sin planet.orbitalAngle)
        |> Vector.scale planet.orbitalRadius

checkCollision : Point -> Float -> Point -> Float -> Bool
checkCollision p1 r1 p2 r2 =
    Vector.dist p1 p2 < r1 + r2

find p = List.filter p >> List.head
