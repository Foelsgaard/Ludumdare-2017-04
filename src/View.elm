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
                |> Collage.move stick.pos

drawPlanet : Planet -> Collage.Form
drawPlanet planet =
    let pos = planetPos planet
    in Collage.filled Color.red (Collage.circle planet.radius)
        |> Collage.move pos

view model =
    let entities =
            List.map drawPlanet model.planets
            ++ List.map drawStick model.sticks

    in Html.div []
        [ Collage.collage 1000 1000 entities |> Element.toHtml
        ]
