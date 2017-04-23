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
               (Random.list 10 randomStick)
               (Random.initialSeed 0)
    |> Tuple.first
    , planets =
          [ Planet
                { radius        = 20
                , mass          = 1000
                , orbitalRadius = 300
                , orbitalAngle  = 0
                , orbitalPeriod = 50 * Time.second
                , maxPopulation = 10
                , inhabitants   = []
                , overpopulated = Nothing
                }
          , Planet
                { radius        = 15
                , mass          = 500
                , orbitalRadius = 150
                , orbitalAngle  = 3
                , orbitalPeriod = 20 * Time.second
                , maxPopulation = 10
                , inhabitants   = []
                , overpopulated = Nothing
                }
          , Planet
                { radius        = 5
                , mass          = 50
                , orbitalRadius = 50
                , orbitalAngle  = 4
                , orbitalPeriod = 3 * Time.second
                , maxPopulation = 5
                , inhabitants   = []
                , overpopulated = Nothing
                }
          , Planet
                { radius        = 10
                , mass          = 250
                , orbitalRadius = 400
                , orbitalAngle  = 2
                , orbitalPeriod = 40 * Time.second
                , maxPopulation = 10
                , inhabitants   = []
                , overpopulated = Nothing
                }
          ]
    , score = 0
    , dragging = Nothing
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
        , dragSubscription model
        ]

positionToPoint : Mouse.Position -> Point
positionToPoint {x, y} = (toFloat x - 500, 500 - toFloat y)

dragSubscription model =
    let subs =
            case model.dragging of
                Nothing ->
                    [ Mouse.downs (positionToPoint >> DragStart)
                    ]
                Just (p, _) ->
                    [ Mouse.ups (\_ -> DragEnd)
                    , Mouse.moves (positionToPoint >> Drag p)
                    ]
    in Platform.Sub.batch subs
