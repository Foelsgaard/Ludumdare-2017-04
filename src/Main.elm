module Main exposing (..)
import Html exposing (Html)
import Mouse exposing (Position)
import Time exposing (Time,every,second)
import AnimationFrame exposing (diffs)
import Collage
import Element
import Color

-- MODEL

type alias Model = 
    {
    sticks : List Stick,
    planets : List Planet
    }

type alias Stick = 
    { 
    pos : Vector,
    vel : Vector,
    acc : Vector
    }

type alias Vector =
    {
    x : Float,
    y : Float
    }

type alias Planet = 
    {
    --pos : Vector,
    radius : Float,
    mass : Float,
    distToStar : Float,
    angle : Float,
    period : Float
    }

g = 10000
maxStickSpeed = 500

--type alias Input =
--    {
--    delta : Time
--    }



-- UPDATE

main =
    Html.program
    { 
    init = ({
        sticks = [
            {
            pos = {x=0,y=0},
            vel ={x=0,y=0},
            acc ={x=0,y=0}
            },
            {
            pos = {x=-50,y=-200},
            vel ={x=200,y=0},
            acc ={x=0,y=0}
            }
            ],
        planets =[
            {
            --pos = {x= 0,y = 0},
            radius = 20,
            mass = 1000,
            distToStar = 200,
            angle = 0,
            period = 10
            },
            {
            --pos = {x= 100,y = 50},
            radius = 15,
            mass = 500,
            distToStar = 100,
            angle = 3,
            period = 4
            }
            ]
        }
        ,Cmd.none), 
    view = view, 
    update = update,
    subscriptions = subscriptions
  }

gravity : Vector -> Planet -> Vector
gravity pos planet =
    mult (norm (sub (planetPos planet) pos))  (g *planet.mass / ((dist (planetPos planet) pos)^2))


dist : Vector -> Vector -> Float
dist pos1 pos2 =
    sqrt ((pos1.x-pos2.x)^2 + (pos1.y-pos2.y)^2)

norm : Vector -> Vector
norm pos =
    {x = pos.x/(dist {x=0,y=0} pos),
    y = pos.y/(dist {x=0,y=0} pos)
    }

planetPos : Planet -> Vector
planetPos planet =
    {x = cos planet.angle *planet.distToStar, y = sin planet.angle *planet.distToStar}

checkCollision : Vector -> Float -> Vector -> Float -> Bool
checkCollision pos1 rad1 pos2 rad2 =
    dist pos1 pos2 < rad1+rad2


drawStick : Stick -> Collage.Form
drawStick stick =
    Collage.filled Color.green (Collage.circle 5) |> Collage.move (stick.pos.x,stick.pos.y)

drawPlanet : Planet -> Collage.Form
drawPlanet planet =
    Collage.filled Color.red (Collage.circle planet.radius) |> Collage.move ((planetPos planet).x,(planetPos planet).y)

updateStick : Time -> List Planet -> Stick -> Stick
updateStick time planets stick = 
    let 
        acc = add stick.acc (List.foldr add {x=0,y=0} (List.map (gravity stick.pos) planets) )
        vel = add stick.vel (mult acc time) 
        pos = add stick.pos (mult velclamp time)        
        velclamp = {x= clamp (negate maxStickSpeed) maxStickSpeed vel.x,y = clamp ( negate maxStickSpeed) maxStickSpeed vel.y}
        collidesWithPlanet : Planet -> Bool
        collidesWithPlanet planet = checkCollision pos 5 (planetPos planet) planet.radius
        collision = find collidesWithPlanet planets
    in
    {stick | 
        vel = case collision of 
            Nothing -> velclamp 
            Just _ -> {x=0,y=0},

            -- then {x=0,y=0} else velclamp
        pos = case collision of 
            Nothing -> pos
            Just planet -> planetPos planet
                --let 
                --    d = dist stick.pos (planetPos planet)
                --    dSmall = d - 5 - planet.radius
                --in
                --add stick.pos (mult  (sub stick.pos (planetPos planet)) (d/dSmall))
                --        mult (sub pos (planetPos planet)) (dist pos (planetPos planet))
        --pos = add stick.pos (mult (if collision then {x=0,y=0} else velclamp) time)
        }

updatePlanet : Time -> Planet -> Planet
updatePlanet time planet = 
    let
        angle = planet.angle + time *2*pi / planet.period
    in
    {planet |
        angle = if angle >= 2*pi then angle - 2*pi else angle
    }

add : Vector -> Vector -> Vector
add pos1 pos2 = {x = pos1.x+pos2.x, y = pos1.y + pos2.y}

sub : Vector -> Vector -> Vector
sub pos1 pos2 = {x = pos1.x-pos2.x, y = pos1.y - pos2.y}

mult : Vector -> Float -> Vector
mult pos scal = {x = scal *pos.x, y = scal *pos.y }

find p = List.filter p >> List.head

type Message = Reset | Tick Time

update : Message -> Model -> (Model,Cmd Message)
update action model =
    (updateHelp action model,Cmd.none)

updateHelp : Message -> Model -> Model
updateHelp action model =
  case action of
    Reset -> model

    Tick time -> 
        {model | 
            sticks = List.map (updateStick time model.planets) model.sticks,
            planets = List.map (updatePlanet time) model.planets
        }




-- SUBSCRIBTIONS

subscriptions model =
    diffs (Time.inSeconds >> Tick)
    --Time.every second (Time.inSeconds >> Tick)

-- VIEWS

view model =
    Html.div []
    [
    --Html.text(toString model.sticks),
    Collage.collage 1000 1000 (List.map drawPlanet model.planets ++List.map drawStick model.sticks ) |> Element.toHtml
    ]

    --Html.text(toString Time.every)