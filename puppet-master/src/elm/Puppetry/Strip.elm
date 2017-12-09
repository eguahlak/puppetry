module Puppetry.Strip exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Puppetry.Lamp as Lamp exposing (Lamp)

-- MODEL

type alias Strip =
  { lampCount : Int
  , activeLamps : List Lamp
  }

getLamp : Strip -> Int -> Lamp
getLamp strip index =
  Lamp.lamp 0 index

type alias Config msg =
  { x1 : Float, y1 : Float
  , x2 : Float, y2 : Float
  , onLampClick : Lamp -> msg
  }

-- VIEW

view : Config msg -> Strip -> Svg msg
view config strip =
  g []
    ([ line
      [ x1 (toString config.x1)
      , y1 (toString config.y1)
      , x2 (toString config.x2)
      , y2 (toString config.y2)
      , strokeWidth "3"
      , stroke "black"
      ] []
    ] ++ (List.map (viewStripLamp config strip) (List.range 0 <| strip.lampCount - 1)))

viewStripLamp : Config msg -> Strip -> Int -> Svg msg
viewStripLamp config strip index =
  let
    dw = (config.x2 - config.x1)/(toFloat strip.lampCount - 1.0)
    dh = (config.y2 - config.y2)/(toFloat strip.lampCount - 1.0)
    stripLenght = toFloat (strip.lampCount - 1)
    lx = config.x1 + dw*(toFloat index)
    ly = config.y1 + dh*(toFloat index)
  in
  Lamp.view { x = lx, y = ly, onClick = config.onLampClick } (getLamp strip index)
