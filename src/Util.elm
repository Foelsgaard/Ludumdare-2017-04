module Util exposing (..)

import Vector exposing (Vector, Point, (.-))

import Model exposing (..)

import Time exposing (Time)

gravity : Point -> Planet -> Vector
gravity p (Planet planet) =
    let acc = (g * planet.mass / ((Vector.dist (planetPos planet) p)^2))
    in Vector.normalize (planetPos planet .- p) |> Vector.scale acc


planetPos planet =
    (cos planet.orbitalAngle, sin planet.orbitalAngle)
        |> Vector.scale planet.orbitalRadius

collision :  {a | pos : Vector, radius : Float}
          -> {b | pos : Vector, radius : Float}
          -> Bool
collision a b =
    Vector.dist a.pos b.pos < a.radius + b.radius

collisionAngle :  {a | pos : Vector, radius : Float}
          -> {b | pos : Vector, radius : Float}
          -> Maybe Float
collisionAngle a b =
    let (dx, dy) = b.pos .- a.pos
    in if Vector.norm (dx, dy) < a.radius + b.radius
       then Just (atan2 dy dx)
       else Nothing

planetStickCollisionAngle planet stick =
    collisionAngle
    { pos = planetPos planet
    , radius = planet.radius
    }
    { pos = stick.pos
    , radius = 5
    }

stickPlanetCollision : Stick -> Planet -> Bool
stickPlanetCollision stick (Planet planet) =
    collision
    { pos = planetPos planet
    , radius = planet.radius
    }
    { pos = stick.pos
    , radius = 5
    }

find p = List.filter p >> List.head

type Either a b
    = Left a
    | Right b

left : Either a b -> Maybe a
left either =
    case either of
        Left a  -> Just a
        Right _ -> Nothing

right : Either a b -> Maybe b
right either =
    case either of
        Left _  -> Nothing
        Right b -> Just b

split : List a -> (a -> Either b c) -> (List b, List c)
split xs f =
    let eithers = List.map f xs
        ls = List.filterMap left eithers
        rs = List.filterMap right eithers
    in (ls, rs)

maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Nothing -> []
        Just x  -> [x]
