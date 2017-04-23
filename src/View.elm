module View exposing (..)

import Util exposing (..)
import Model exposing (..)
import Vector

import Collage
import Color
import Html
import Element
import Dict

drawStick : Stick -> Collage.Form
drawStick stick =
    Collage.filled Color.green (Collage.circle 5)
                |> Collage.move stick.pos

drawPlanet : Planet -> Collage.Form
drawPlanet (Planet planet) =
    let pos = planetPos planet
        drawInhabitant angle =
            Collage.filled Color.green (Collage.circle 5)
                |> Collage.move
                   (Vector.scale (5 + planet.radius) (cos angle, sin angle))

        hue =
            case planet.overpopulated of
                Nothing -> 2 / 3 * pi
                           * (1 - toFloat (List.length (planet.inhabitants))
                                  / toFloat planet.maxPopulation)
                           |> clamp 0 (2 / 3 * pi)
                Just t  -> 0.5 * 2 / 3 * pi * (1 - t / overpopulationTimer)
    in Collage.group
        (Collage.filled (Color.hsl hue 1 0.5) (Collage.circle planet.radius)
        :: (List.map drawInhabitant planet.inhabitants))
        |> Collage.move pos

view model =
    let entities =
            List.map drawStick model.sticks
            ++ List.map drawPlanet model.planets

    in Html.div []
        [ Collage.collage 1000 1000 entities |> Element.toHtml
        ]
