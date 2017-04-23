module Update exposing (..)

import Vector exposing (Point, (.+), (.-))
import Util exposing (..)
import Model exposing (..)
import Random exposing (Generator)

import Time exposing (Time)
import Dict exposing (Dict)

import Collage


updatePlanet : Time -> List Stick -> Planet -> (Planet,List Point)
updatePlanet dt sticks (Planet planet) =
    let newOrbitalAngle =
            planet.orbitalAngle + dt * 2 * pi / planet.orbitalPeriod
        collidingSticks =
            List.filterMap (planetStickCollisionAngle planet) sticks
        potentialNewInhabitants = planet.inhabitants ++ collidingSticks

        (newInhabitants, newOverpopulated, explodingPoints) =
            case ( planet.overpopulated
                 , List.length potentialNewInhabitants
                     >= planet.maxPopulation
                 ) of
                (Nothing, True) -> ( [], Just overpopulationTimer, [planetPos planet])
                (op, True)      -> ( []
                                   , Maybe.map (\t -> (t - dt)) op
                                   , []
                                   )
                (Just t, False) -> ( []
                                   , Just (t - dt)
                                   , []
                                   )
                (op, False)     -> (potentialNewInhabitants, op,[])
    in (Planet { planet
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
              },explodingPoints)

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    (updateHelp msg model, Cmd.none)

updateHelp : Message -> Model -> Model
updateHelp msg model =
  case msg of
    Reset -> model

    Tick dt ->
      let 
        (newSticks,points) = split model.sticks (updateStick dt model.planets)
        (newPlanets,planetPoints) = List.map (updatePlanet dt model.sticks) model.planets |> List.unzip 
        --newParticles = List.concatMap (createExplosion 1) (points ++ List.concat planetPoints)
        newStickParticles = List.map (createExplosion 1) points
        newPlanetParticles = List.map (createExplosion 100) (List.concat planetPoints)
        newParticles = List.concat (newStickParticles ++ newPlanetParticles)

      in { model
            | particles = 
                List.filterMap (updateParticle dt) model.particles ++ newParticles
            , sticks=
                newSticks
            , planets =
                newPlanets
            , score = List.length model.sticks
        }
    DragStart p ->
        { model
            | dragging = Just (p, p)
        }

    Drag p1 p2 ->
        { model
            | dragging = Just (p1, p2)
        }

    DragEnd ->
        { model
            | dragging = Nothing
        }

createExplosion : Int -> Point -> List Particle
createExplosion numParticles pos =
                  Random.step
                  (Random.list numParticles (generateParticle pos))
                  (Random.initialSeed 0)
                  |> Tuple.first


generateParticle : Point -> Generator Particle 
generateParticle genPos =
  let mkParticle vel =
      { pos = genPos
      , vel = vel
      , lifetime = 0.5* Time.second
      }
      randomVel = Random.pair
                    (Random.float -0.1 0.1)
                    (Random.float -0.1 0.1)
  in Random.map mkParticle randomVel


updateParticle : Time -> Particle -> Maybe Particle
updateParticle dt particle =
    let
        newVel = particle.vel
        newPos = particle.pos .+ Vector.scale dt particle.vel
        newLifetime = particle.lifetime - dt
    in  if newLifetime >= 0
        then Just { pos = newPos
                  , vel = newVel
                  , lifetime = newLifetime
                  }
        else Nothing

updateStick : Time -> List Planet -> Stick -> Either Stick Point
updateStick dt planets stick =

    let acc = Vector.sum (List.map (gravity stick.pos) planets)

        newVel = stick.vel .+ Vector.scale dt acc
               |> Vector.clamp (negate maxSpeed) maxSpeed

        newPos = stick.pos .+ Vector.scale dt newVel

        newAngle = stick.angle +0.1

    in if List.any (stickPlanetCollision stick) planets
       then Right stick.pos
       else Left { stick
                     | vel = newVel
                     , pos = newPos
                     , angle = newAngle
                 }

type Message
    = Reset
    | Tick Time
    | DragStart Point
    | Drag Point Point
    | DragEnd
