module View exposing (..)

import Util exposing (..)
import Model exposing (..)
import Vector exposing (Point)

import Text
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
        Collage.group   [ Collage.filled stickColor (Collage.circle 3) |> Collage.move (0,3)
                    , Collage.filled stickColor (Collage.rect 2 10)
                    , Collage.filled stickColor (Collage.rect 2 5) |> Collage.rotate (degrees 25) |> Collage.move (1,-7)
                    , Collage.filled stickColor (Collage.rect 2 5) |> Collage.rotate (degrees -25) |> Collage.move (-1,-7)
                    , Collage.filled stickColor (Collage.rect 2 4) |> Collage.rotate (degrees 65) |> Collage.move (1,-2)
                    , Collage.filled stickColor (Collage.rect 2 4) |> Collage.rotate (degrees -65) |> Collage.move (-1,-2)
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
            case (planet.overpopulated, planet.maxPopulation) of
                (_, 0) -> 0
                (Nothing, _) ->
                    2 / 3 * pi
                        * (1 - toFloat (List.length (planet.inhabitants))
                               / toFloat planet.maxPopulation)
                            |> clamp 0 (2 / 3 * pi)
                (Just t, _) ->
                    0.5 * 2 / 3 * pi * (1 - t / overpopulationTimer)
    in  (Collage.filled (Color.hsl hue 1 0.5) (Collage.circle planet.radius))
        :: (Collage.text
                (case (planet.overpopulated, planet.maxPopulation) of
                    (_, 0) -> Text.fromString "!!!"
                    (Nothing, _) ->
                        (Text.fromString
                             (String.concat
                                  [ toString (List.length planet.inhabitants)
                                  , "/"
                                  , toString planet.maxPopulation
                                  ]
                             ))
                    (Just _, _) -> Text.fromString "!!!"
                )
           )
        :: (List.map drawInhabitant planet.inhabitants)
        |> Collage.group
        |> Collage.move pos

--drawOrbits : List Planet -> Collage.Form
--drawOrbits planets =
--    Collage.traced (Collage.solid Color.black) (Collage.segment (0,0) (fromPolar (planets.orbitalRadius,0)))

gradStar : Color.Gradient
gradStar =
  Color.radial (0,0) 5 (0,0) 25
    [ (  0, Color.rgb  244 150 1)
    , (0.5, Color.rgb  228 199 0)
    , (  1, Color.rgba 228 199 0 0)
    ]

view model =
    let entities =

            [Collage.filled (Color.hsl (1.2*pi) 0.5 0.8) (Collage.rect screenWidth screenHeight) |> Collage.move (0,0)]
            ++ [Collage.gradient gradStar (Collage.circle 200)|> Collage.move(0,0)]
            ++ List.map drawStick model.sticks
            ++ List.map drawPlanet model.planets
            ++ List.map drawParticle model.particles
            ++ [(Collage.text (Text.fromString "Current score: ")
                |> Collage.move scoreboardPos)]
            ++ [(Collage.text (Text.fromString (toString model.score))
                |> Collage.move
                     (Vector.sub scoreboardPos scoreboardSpacing))]
            ++ [(Collage.text (Text.fromString ("Every second you get:"))
                |> Collage.move
                     (Vector.sub scoreboardPos
                          (Vector.scale 2 scoreboardSpacing)))]
            ++ [(Collage.text
                     (Text.fromString ("+"++(toString model.deltaScore)))
                |> Collage.move
                     (Vector.sub scoreboardPos
                          (Vector.scale 3 scoreboardSpacing)))]
            ++ [(Collage.text
                     (Text.fromString
                          ("from the inhabitants of your planets!"))
                |> Collage.move (Vector.sub scoreboardPos
                                     (Vector.scale 4 scoreboardSpacing)))]
            ++ [(Collage.text
                     (Text.fromString
                          ("Don't let your planets get overpopulated!"))
                |> Collage.move (Vector.sub scoreboardPos
                                     (Vector.scale 5 scoreboardSpacing)))]
            ++ if model.gameOver
               then [ Text.fromString "Game Over"
                    |> Text.style
                          { typeface = [ "Arial" ]
                          , height = Just 40
                          , color = Color.black
                          , bold = True
                          , italic = False
                          , line = Nothing
                          }
                    |> Collage.text
                    |> Collage.move (0, screenHeight * 0.3)
                    ,  Text.fromString
                        (String.concat
                             [ "Final score: "
                             , toString model.score
                             ])
                    |> Text.style
                          { typeface = [ "Arial" ]
                          , height = Just 40
                          , color = Color.black
                          , bold = True
                          , italic = False
                          , line = Nothing
                          }
                    |> Collage.text
                    |> Collage.move (0, screenHeight * 0.2)
                    ]
               else []

    in Html.div []
        [ Collage.collage 1200 800 entities |> Element.toHtml
        ]
