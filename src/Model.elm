module Model exposing (..)

import Vector exposing (Vector, Point)

import Time exposing (Time)
import Collage
import Dict exposing (Dict)

-- Types

type alias Model =
    { particles : List Particle
    , sticks  : List Stick
    , planets : List Planet
    , score : Int
    , deltaScore : Int
    , dragging : Maybe (Point, Point)
    }

type alias Key = Int

type alias Stick =
    { pos   : Vector
    , vel   : Vector
    , angle : Float
    }

type alias Particle =
    { pos      : Vector
    , vel      : Vector
    , lifetime : Float
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
    , textString    : String
    }

-- Constants

overpopulationTimer = 2 * Time.second

g = 0.01
maxSpeed = 50000 * pixelsPerSecond

pixelsPerSecond = 1 / Time.second

particleMaxLifetime = 5 

scoreboardPos = (-400,465)
scoreboardSpacing = (0,15)
