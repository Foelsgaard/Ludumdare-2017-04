module Main exposing (..)

import Model exposing (..)
import Util exposing (..)
import Update exposing (..)
import View exposing (..)

import Time exposing (Time)
import AnimationFrame
import Html

-- Main function

main = Html.program
       { init          = (initialModel, Cmd.none)
       , view          = view
       , update        = update
       , subscriptions = subscriptions
       }

initialModel =
    { sticks =
          [ { pos = (0, 0)
            , vel = (0, 0)
            }
          , { pos = (-50, -200)
            , vel = (100 * pixelsPerSecond, 0)
            }
          ]
    , planets =
          [ { radius        = 20
            , mass          = 1000
            , orbitalRadius = 200
            , orbitalAngle  = 0
            , orbitalPeriod = 10 * Time.second
            }
          , { radius        = 15
            , mass          = 500
            , orbitalRadius = 100
            , orbitalAngle  = 3
            , orbitalPeriod = 4 * Time.second
            }
          ]
    }

-- SUBSCRIBTIONS

subscriptions model =
    AnimationFrame.diffs Tick
