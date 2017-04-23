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

-- Main function

main = Html.program
       { init          = (initialModel, Cmd.none)
       , view          = view
       , update        = update
       , subscriptions = subscriptions
       }

initialModel =
    { particles = Random.step
                  (Random.list 300 (generateParticle (0,0)))
                  (Random.initialSeed 0)
    |> Tuple.first
    , sticks = Random.step
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
                , maxPopulation = 100
                , inhabitants   = []
                , overpopulated = Nothing
                }
          ]
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

--generateParticles : List Vector -> Generator (List Particle)
--generateParticles positions = 

-- SUBSCRIBTIONS

maxDiffLength = 20 * Time.millisecond

subscriptions model =
    AnimationFrame.diffs (min maxDiffLength >> Tick)
