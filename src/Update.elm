module Update exposing (..)

import Util exposing (..)
import Model exposing (..)

import Time exposing (Time)

updatePlanet : Time -> Planet -> Planet
updatePlanet time planet =
    let newAngle = planet.angle + time * 2 * pi / planet.period
    in { planet
           | angle = if newAngle >= 2 * pi
                     then newAngle - 2 * pi
                     else newAngle
       }

type Message = Reset | Tick Time

update : Message -> Model -> (Model, Cmd Message)
update action model =
    (updateHelp action model, Cmd.none)

updateHelp : Message -> Model -> Model
updateHelp action model =
  case action of
    Reset -> model

    Tick time ->
        { model
            | sticks  = List.map (updateStick time model.planets) model.sticks
            , planets = List.map (updatePlanet time) model.planets
        }

updateStick : Time -> List Planet -> Stick -> Stick
updateStick time planets stick =
    let acc = vSum (List.map (gravity stick.pos) planets)

        newVel = stick.vel .+ scale time acc
            |> vClamp (negate maxSpeed) maxSpeed

        newPos = stick.pos .+ scale time newVel

        collidesWithPlanet planet =
            checkCollision newPos 5 (planetPos planet) planet.radius

        collidingPlanet = find collidesWithPlanet planets

    in case collidingPlanet of
           Nothing ->
               { stick
                   | vel = newVel
                   , pos = newPos
               }
           Just planet ->
               { stick
                   | vel = {x = 0, y = 0}
                   , pos =
                     let dir = normalize (stick.pos .- planetPos planet)
                     in scale (5 + planet.radius) dir .+ planetPos planet
               }
