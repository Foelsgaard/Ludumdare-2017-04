module View exposing (..)

import Util exposing (..)
import Model exposing (..)

import Collage
import Color
import Html
import Element

drawStick : Stick -> Collage.Form
drawStick stick =
    Collage.filled Color.green (Collage.circle 5)
        |> Collage.move (stick.pos.x,stick.pos.y)

drawPlanet : Planet -> Collage.Form
drawPlanet planet =
    Collage.filled Color.red (Collage.circle planet.radius)
        |> Collage.move ((planetPos planet).x,(planetPos planet).y)

view model =
    let entities =
            List.map drawPlanet model.planets
            ++ List.map drawStick model.sticks

    in Html.div []
        [ Collage.collage 1000 1000 entities |> Element.toHtml
        ]
