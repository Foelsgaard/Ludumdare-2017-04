module Model exposing (..)

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

type alias Vector = (Float, Float)

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
