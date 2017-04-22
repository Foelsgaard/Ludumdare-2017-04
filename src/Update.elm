module Update exposing (..)

import Util exposing (..)
import Model exposing (..)

import Time exposing (Time)

updatePlanet : Time -> Planet -> Planet
updatePlanet dt planet =
    let newOrbitalAngle =
            planet.orbitalAngle + dt * 2 * pi / planet.orbitalPeriod
    in { planet
           | orbitalAngle =
             if newOrbitalAngle >= 2 * pi
             then newOrbitalAngle - 2 * pi
             else newOrbitalAngle
       }

type Message = Reset | Tick Time

update : Message -> Model -> (Model, Cmd Message)
update action model =
    (updateHelp action model, Cmd.none)

updateHelp : Message -> Model -> Model
updateHelp action model =
  case action of
    Reset -> model

    Tick dt ->
        { model
            | sticks  = List.map (updateStick dt model.planets) model.sticks
            , planets = List.map (updatePlanet dt) model.planets
        }

updateStick : Time -> List Planet -> Stick -> Stick
updateStick dt planets stick =
    let acc = vSum (List.map (gravity stick.pos) planets)

        newVel = stick.vel .+ vScale dt acc
            |> vClamp (negate maxSpeed) maxSpeed

        newPos = stick.pos .+ vScale dt newVel

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
                   | vel = (0, 0)
                   , pos =
                     let dir = normalize (stick.pos .- planetPos planet)
                     in vScale (5 + planet.radius) dir .+ planetPos planet
               }
