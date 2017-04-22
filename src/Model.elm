module Model exposing (..)

import Vector exposing (Vector)

import Time

-- Types

type alias Model =
    { sticks  : List Stick
    , planets : List Planet
    }

type alias Stick =
    { pos : Vector
    , vel : Vector
    }

type alias Planet =
    { radius        : Float
    , mass          : Float
    , orbitalRadius : Float
    , orbitalAngle  : Float
    , orbitalPeriod : Float
    }

-- Constants

g = 0.01
maxSpeed = 500

pixelsPerSecond = 1 / Time.second
