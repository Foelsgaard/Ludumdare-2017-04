module Model exposing (..)

-- Types

type alias Model =
    { sticks  : List Stick
    , planets : List Planet
    }

type alias Stick =
    { pos : Vector
    , vel : Vector
    }

type alias Vector =
    { x : Float
    , y : Float
    }

type alias Planet =
    { radius     : Float
    , mass       : Float
    , distToStar : Float
    , angle      : Float
    , period     : Float
    }

-- Constants

g = 10000
maxSpeed = 500

