module View exposing (..)

import Util exposing (..)
import Model exposing (..)
import Vector exposing (Point)
import Text exposing (..)

import Collage
import Color
import Html
import Element
import Dict

drawStick : Stick -> Collage.Form
drawStick stick = 
            drawStickFigure stick.angle
             |> Collage.move stick.pos

drawStickFigure : Float -> Collage.Form
drawStickFigure angle =
        Collage.group   [ Collage.filled Color.black (Collage.circle 3) |> Collage.move (0,3)
                    , Collage.filled Color.black (Collage.rect 2 10)
                    , Collage.filled Color.black (Collage.rect 2 5) |> Collage.rotate (degrees 25) |> Collage.move (1,-7)
                    , Collage.filled Color.black (Collage.rect 2 5) |> Collage.rotate (degrees -25) |> Collage.move (-1,-7)
                    , Collage.filled Color.black (Collage.rect 2 4) |> Collage.rotate (degrees 65) |> Collage.move (1,-2)
                    , Collage.filled Color.black (Collage.rect 2 4) |> Collage.rotate (degrees -65) |> Collage.move (-1,-2)
                    ]
                    |> Collage.rotate (degrees -90)
                    |> Collage.rotate angle


drawParticle : Particle -> Collage.Form
drawParticle particle =
        Collage.filled (Color.hsl particle.lifetime 1 0.5) (Collage.circle 1) |> Collage.move particle.pos
        --Collage.filled Collage.black (Collage.circle 1) |> Collage.move particle.pos


drawPlanet : Planet -> Collage.Form
drawPlanet (Planet planet) =
    let pos = planetPos planet
        drawInhabitant angle =
            drawStickFigure angle
                |> Collage.move
                   (Vector.scale (5 + planet.radius) (cos angle, sin angle))

        hue =
            case planet.overpopulated of
                Nothing -> 2 / 3 * pi
                           * (1 - toFloat (List.length (planet.inhabitants))
                                  / toFloat planet.maxPopulation)
                           |> clamp 0 (2 / 3 * pi)
                Just t  -> 0.5 * 2 / 3 * pi * (1 - t / overpopulationTimer)
    in  [ (Collage.filled (Color.hsl hue 1 0.5) (Collage.circle planet.radius))
        , (Collage.text (Text.fromString (toString (List.length planet.inhabitants))))] 
        ++ (List.map drawInhabitant planet.inhabitants)
        |> Collage.group
        |> Collage.move pos

view model =
    let entities =
            List.map drawStick model.sticks
            ++ List.map drawPlanet model.planets
            ++ List.map drawParticle model.particles
            ++ maybeToList (Maybe.map drawDrag model.dragging)

    in Html.div []
        [ Collage.collage 1000 1000 entities |> Element.toHtml
        ]

drawDrag : (Point, Point) -> Collage.Form
drawDrag (p1, p2) =
    let linestyle =
            { color = Color.blue
            , width = 3
            , cap   = Collage.Flat
            , join  = Collage.Smooth
            , dashing = []
            , dashOffset = 0
            }
        path = Collage.segment p1 p2
    in Collage.traced linestyle path
