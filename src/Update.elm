module Update exposing (..)

import Vector exposing ((.+), (.-))
import Util exposing (..)
import Model exposing (..)

import Time exposing (Time)
import Dict exposing (Dict)

import Collage

updatePlanet : Time -> List Stick -> Planet -> Planet
updatePlanet dt sticks (Planet planet) =
    let newOrbitalAngle =
            planet.orbitalAngle + dt * 2 * pi / planet.orbitalPeriod
        collidingSticks =
            List.filterMap (planetStickCollisionAngle planet) sticks
        potentialNewInhabitants = planet.inhabitants ++ collidingSticks

        (newInhabitants, newOverpopulated) =
            case ( planet.overpopulated
                 , List.length potentialNewInhabitants
                     >= planet.maxPopulation
                 ) of
                (Nothing, True) -> ( [], Just overpopulationTimer)
                (op, True)      -> ( []
                                   , Maybe.map (\t -> (t - dt)) op
                                   )
                (Just t, False) -> ( []
                                   , Just (t - dt)
                                   )
                (op, False)     -> (potentialNewInhabitants, op)
    in Planet { planet
                  | orbitalAngle =
                    if newOrbitalAngle >= 2 * pi
                    then newOrbitalAngle - 2 * pi
                    else newOrbitalAngle
                  , inhabitants = newInhabitants
                  , overpopulated =
                      case newOverpopulated of
                          Just t -> if t <= 0
                                    then Nothing
                                    else Just t
                          op     -> op
              }

type Message = Reset | Tick Time

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    (updateHelp msg model, Cmd.none)

updateHelp : Message -> Model -> Model
updateHelp msg model =
  case msg of
    Reset -> model

    Tick dt ->
        { model
            | sticks =
                List.filterMap (updateStick dt model.planets) model.sticks
            , planets =
                List.map (updatePlanet dt model.sticks) model.planets
        }

updateStick : Time -> List Planet -> Stick -> Maybe Stick
updateStick dt planets stick =

    let acc = Vector.sum (List.map (gravity stick.pos) planets)

        newVel = stick.vel .+ Vector.scale dt acc
               |> Vector.clamp (negate maxSpeed) maxSpeed

        newPos = stick.pos .+ Vector.scale dt newVel

    in if List.any (stickPlanetCollision stick) planets
       then Nothing
       else Just { stick
                     | vel = newVel
                     , pos = newPos
                 }
