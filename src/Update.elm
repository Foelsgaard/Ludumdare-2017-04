module Update exposing (..)

import Vector exposing (Point, (.+), (.-))
import Util exposing (..)
import Model exposing (..)
import Random exposing (Generator)

import Time exposing (Time)
import Dict exposing (Dict)

import Collage


updatePlanet : Time
             -> List Stick
             -> Planet
             -> (Planet, List Point)
updatePlanet dt sticks (Planet planet) =
    let newOrbitalAngle =
            planet.orbitalAngle + dt * 2 * pi / planet.orbitalPeriod
        collidingSticks =
            List.filterMap (planetStickCollisionAngle planet) sticks
        potentialNewInhabitants = planet.inhabitants ++ collidingSticks

        (newInhabitants, newOverpopulated, explodingPoints, newTextString, newMaxPopulation) =
            case ( planet.overpopulated
                 , List.length potentialNewInhabitants
                     >= planet.maxPopulation
                 ) of
                (Nothing, True) -> ( [], Just overpopulationTimer, [planetPos planet], "!!!", max (planet.maxPopulation-1) 0 )
                (Just t, _) -> ( []
                               , Just (t - dt)
                               , []
                               , "!!!"
                               , planet.maxPopulation
                               )
                (op, False) -> ( potentialNewInhabitants
                               , op
                               , []
                               , (toString (List.length planet.inhabitants))++"/"++toString planet.maxPopulation
                               , planet.maxPopulation
                               )
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
                  , textString = newTextString
                  , maxPopulation = newMaxPopulation
              },explodingPoints)

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        Tick dt ->
            ( updateHelp msg model
            , if model.untilPop - dt <= 0
              then (List.concatMap popInhabitants model.planets
                   |> List.map (Random.generate Pop)
                   |> Cmd.batch)
              else Cmd.none
            )
        msg -> (updateHelp msg model, Cmd.none)

updateHelp : Message -> Model -> Model
updateHelp msg model =
  case msg of
    Reset -> model

    Pop Nothing -> model
    Pop (Just stick) ->
        { model
            | sticks = stick :: model.sticks
        }

    Tick dt ->
      let
        (newSticks,points) = split model.sticks (updateStick dt model.planets)
        (newPlanets,planetPoints) = List.map (updatePlanet dt model.sticks) model.planets |> List.unzip 
        --newParticles = List.concatMap (createExplosion 1) (points ++ List.concat planetPoints)
        newStickParticles = List.map (createExplosion 1) points
        newPlanetParticles = List.map (createExplosion 10) (List.concat planetPoints)
        newParticles = List.concat (newStickParticles ++ newPlanetParticles)

        pop = model.untilPop - dt <= 0
        newScore =
            if pop
            then model.score + model.deltaScore
            else model.score

      in { model
            | particles = 
                List.filterMap (updateParticle dt) model.particles ++ newParticles
            , sticks=
                newSticks
            , planets =
                newPlanets
            --, score = model.score+model.deltaScore
            --, deltaScore = List.length model.sticks + List.sum(List.map (\(Planet planet) -> List.length planet.inhabitants) model.planets)
            , deltaScore = List.sum(List.map (\(Planet planet) -> List.length planet.inhabitants) model.planets)
             , score = newScore
             , untilPop =
                 if pop
                 then model.untilPop - dt + Time.second
                 else model.untilPop - dt
        }
    Flick p ->
        let (newPlanets, flickedSticks) =
                List.map (flickInhabitant p) model.planets |> List.unzip
        in { model
               | planets = newPlanets
               , sticks = model.sticks ++ List.concat flickedSticks
           }

inhabitantPos : Point -> Float -> Float -> Point
inhabitantPos pos radius angle =
    (Vector.scale (5 + radius) (cos angle, sin angle)) .+ pos

flickInhabitant : Point -> Planet -> (Planet, List Stick)
flickInhabitant p (Planet planet) =
    let (flicked, remaining) =
            List.partition
                (\angle ->
                     Vector.dist
                     (inhabitantPos
                          (planetPos planet)
                          planet.radius
                          angle)
                     p <= 10)
                planet.inhabitants

        angleToStick angle =
            let p = inhabitantPos (planetPos planet) planet.radius angle
                dir = p .- planetPos planet |> Vector.normalize
            in { pos = Vector.scale 20 dir .+ p
               , vel = Vector.scale (600 * pixelsPerSecond) dir
               , angle = angle
               }
    in ( Planet { planet
                    | inhabitants = remaining
                }
       , List.map angleToStick flicked
       )

popInhabitants : Planet -> List (Generator (Maybe Stick))
popInhabitants (Planet planet) =
    List.map (popInhabitant planet) planet.inhabitants

popInhabitant planet angle =
    let b = Random.map (\r -> r <= 0.2) (Random.float 0 1)
        angleToStick angle pops fuzz =
            let p = inhabitantPos (planetPos planet) planet.radius angle
                dir = p .- planetPos planet
                    |> Vector.normalize
                    |> Vector.rotate fuzz
            in if pops
               then Just { pos = Vector.scale 20 dir .+ p
                         , vel = Vector.scale (300 * pixelsPerSecond) dir
                         , angle = angle
                         }
               else Nothing
    in Random.map2 (angleToStick angle) b (Random.float -1 1)

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

    let acc =(Vector.sum (List.map (gravity stick.pos) planets)) 
              .+ gravity stick.pos ( Planet -- Gravity from the star in the middle
                { radius        = 2
                , mass          = 200
                , orbitalRadius = 0
                , orbitalAngle  = 0
                , orbitalPeriod = 0 * Time.second
                , maxPopulation = 0
                , inhabitants   = []
                , overpopulated = Nothing
                , textString    = "Star"
                }
                )

        newVel = stick.vel .+ Vector.scale dt acc
               |> Vector.clamp (negate maxSpeed) maxSpeed

        newPos = stick.pos .+ Vector.scale dt newVel

        newAngle = stick.angle +0.1

    in if List.any (stickPlanetCollision stick) planets || Vector.dist stick.pos (0,0) <= 20 || Vector.dist stick.pos (0,0) >= 1000 -- Max distance away from origin: 1000
       then Right stick.pos
       else Left { stick
                     | vel = newVel
                     , pos = newPos
                     , angle = newAngle
                 }

type Message
    = Reset
    | Tick Time
    | Flick Point
    | Pop (Maybe Stick)
