module Main exposing (..)

import Model exposing (..)
import Util exposing (..)
import Update exposing (..)
import View exposing (..)
import Vector exposing (..)

import Time exposing (Time)
import AnimationFrame
import Html
import Dict
import Random exposing (Generator)
import Platform.Sub
import Mouse

-- Main function

main = Html.program
       { init          = (initialModel, Cmd.none)
       , view          = view
       , update        = update
       , subscriptions = subscriptions
       }

initialModel =
    { particles = Random.step
                  (Random.list 0 (generateParticle (0,0)))
                  (Random.initialSeed 0)
    |> Tuple.first
    , sticks = Random.step
               (Random.list 0 randomStick)
               (Random.initialSeed 0)
    |> Tuple.first
    , planets =
          [ Planet
                { radius        = 30
                , mass          = 500
                , orbitalRadius = 300
                , orbitalAngle  = 0
                , orbitalPeriod = 35 * Time.second
                , maxPopulation = 7
                , inhabitants   = []
                , overpopulated = Nothing
                }
          , Planet
                { radius        = 24
                , mass          = 300
                , orbitalRadius = 250
                , orbitalAngle  = 3
                , orbitalPeriod = 30 * Time.second
                , maxPopulation = 6
                , inhabitants   = [pi * 0.5]
                , overpopulated = Nothing
                }
          , Planet
                { radius        = 15
                , mass          = 100
                , orbitalRadius = 200
                , orbitalAngle  = 4
                , orbitalPeriod = 25 * Time.second
                , maxPopulation = 3
                , inhabitants   = []
                , overpopulated = Nothing
                }
          , Planet
                { radius        = 18
                , mass          = 220
                , orbitalRadius = 150
                , orbitalAngle  = 9
                , orbitalPeriod = 20 * Time.second
                , maxPopulation = 5
                , inhabitants   = []
                , overpopulated = Nothing
                }
          , Planet
                { radius        = 10
                , mass          = 280
                , orbitalRadius = 100
                , orbitalAngle  = 1.5 * pi
                , orbitalPeriod = 15 * Time.second
                , maxPopulation = 2
                , inhabitants   = []
                , overpopulated = Nothing
                }
          ]
    , score = 0
    , deltaScore = 0
    , seed = Random.initialSeed 0
    , untilPop = Time.second
    , timeElapsed = 0
    , gameOver = False
    }

randomStick : Generator Stick
randomStick =
    let mkStick pos vel angle =
            { pos = pos
            , vel = vel
            , angle = angle
            }
        randomPos = Random.pair
                    (Random.float -500 500)
                    (Random.float -500 500)
        randomVel =
            let (low, high) = (-1 * pixelsPerSecond, 1 * pixelsPerSecond)
            in Random.pair
                (Random.float low high)
                (Random.float low high)

        randomAngle = Random.float 0 (2*pi)
    in Random.map3 mkStick randomPos randomVel randomAngle

generateParticle : Vector -> Generator Particle
generateParticle genPos =
  let mkParticle vel =
      { pos = genPos
      , vel = vel
      , lifetime = 0.5* Time.second
      }
      randomVel = Random.pair
                    (Random.float -1 1)
                    (Random.float -1 1)
  in Random.map mkParticle randomVel

-- SUBSCRIBTIONS

maxDiffLength = 20 * Time.millisecond

subscriptions model =
    Platform.Sub.batch
        [ AnimationFrame.diffs (min maxDiffLength >> Tick)
        , Mouse.clicks (positionToPoint >> Flick)
        ]

positionToPoint : Mouse.Position -> Point
positionToPoint {x, y} = ( toFloat x - screenWidth * 0.5
                         , screenHeight * 0.5 - toFloat y
                         )

