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
          [ { pos = {x = 0, y = 0}
            , vel = {x = 0, y = 0}
            }
          , { pos = {x = -50, y = -200}
            , vel = {x = 200, y = 0}
            }
          ]
    , planets =
          [ { radius     = 20
            , mass       = 1000
            , distToStar = 200
            , angle      = 0
            , period     = 10
            }
          , { radius     = 15
            , mass       = 500
            , distToStar = 100
            , angle      = 3
            , period     = 4
            }
          ]
    }

-- SUBSCRIBTIONS

subscriptions model =
    AnimationFrame.diffs (Time.inSeconds >> Tick)
