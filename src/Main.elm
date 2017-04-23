module Main exposing (..)

import Model exposing (..)
import Util exposing (..)
import Update exposing (..)
import View exposing (..)

import Time exposing (Time)
import AnimationFrame
import Html
import Dict
import Random exposing (Generator)

-- Main function

main = Html.program
       { init          = (initialModel, Cmd.none)
       , view          = view
       , update        = update
       , subscriptions = subscriptions
       }

initialModel =
    { sticks = Random.step
               (Random.list 200 randomStick)
               (Random.initialSeed 0)
    |> Tuple.first
    , planets =
          [ Planet
                { radius        = 20
                , mass          = 1000
                , orbitalRadius = 200
                , orbitalAngle  = 0
                , orbitalPeriod = 50 * Time.second
                , maxPopulation = 10
                , inhabitants   = []
                , overpopulated = Nothing
                }
          , Planet
                { radius        = 15
                , mass          = 500
                , orbitalRadius = 100
                , orbitalAngle  = 3
                , orbitalPeriod = 20 * Time.second
                , maxPopulation = 8
                , inhabitants   = []
                , overpopulated = Nothing
                }
          ]
    }

randomStick : Generator Stick
randomStick =
    let mkStick pos vel =
            { pos = pos
            , vel = vel
            }
        randomPos = Random.pair
                    (Random.float -500 500)
                    (Random.float -500 500)
        randomVel =
            let (low, high) = (-1 * pixelsPerSecond, 1 * pixelsPerSecond)
            in Random.pair
                (Random.float low high)
                (Random.float low high)
    in Random.map2 mkStick randomPos randomVel

-- SUBSCRIBTIONS

maxDiffLength = 20 * Time.millisecond

subscriptions model =
    AnimationFrame.diffs (min maxDiffLength >> Tick)
