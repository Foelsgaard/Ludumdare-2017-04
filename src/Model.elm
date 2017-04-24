module Model exposing (..)

import Vector exposing (Vector, Point)

import Time exposing (Time)
import Collage
import Dict exposing (Dict)
import Random exposing (Generator)
import Color exposing (..)

-- Types

type alias Model =
    { particles : List Particle
    , sticks  : List Stick
    , planets : List Planet
    , score : Int
    , deltaScore : Int
    , seed : Random.Seed
    , untilPop : Time
    , timeElapsed : Int
    , gameOver : Bool
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
    }

-- Constants

overpopulationTimer = 2 * Time.second

g = 0.01
maxSpeed = 50000 * pixelsPerSecond

pixelsPerSecond = 1 / Time.second

particleMaxLifetime = 5

screenWidth = 1200
screenHeight = 800

scoreboardPos = (screenWidth * -0.4 , screenHeight * 0.45)
scoreboardSpacing = (0,15)

stickColor = Color.black  -- (Color.hsl 0 0 0.5)
 
