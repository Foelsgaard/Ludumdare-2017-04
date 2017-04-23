module Model exposing (..)

import Vector exposing (Vector)

import Time exposing (Time)
import Collage
import Dict exposing (Dict)

-- Types

type alias Model =
    { sticks  : List Stick
    , planets : List Planet
    }

type alias Key = Int

type alias Stick =
    { pos   : Vector
    , vel   : Vector
    }

type Planet = Planet
    { radius        : Float
    , mass          : Float
    , orbitalRadius : Float
    , orbitalAngle  : Float
    , orbitalPeriod : Float
    , inhabitants   : List Float
    , maxPopulation : Int
    , overpopulated : Maybe Time
    }

-- Constants

overpopulationTimer = 10 * Time.second

g = 0.001
maxSpeed = 50000 * pixelsPerSecond

pixelsPerSecond = 1 / Time.second
