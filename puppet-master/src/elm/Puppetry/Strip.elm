module Puppetry.Strip exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Encode as JE
import Puppetry.Lamp as Lamp exposing (Lamp)
import Puppetry.ColorSelector as Selector

-- MODEL

type alias Strip =
  { code : Char
  , lampCount : Int
  , activeLamps : List Lamp
  , selectedIndex : Maybe Int
  }

jsValue : Strip -> JE.Value
jsValue strip =
  JE.list (List.map Lamp.jsValue strip.activeLamps)

getLamp : Strip -> Int -> Lamp
getLamp strip index =
  getOrCreateLamp (Lamp.lamp -1) strip.activeLamps index

getOrCreateLamp : Lamp -> List Lamp -> Int -> Lamp
getOrCreateLamp lastLamp lamps index =
  case lamps of
    [] -> Lamp.passiveLamp lastLamp.selector.color index
    activeLamp :: remainingActiveLamps ->
      if index > activeLamp.index then
        getOrCreateLamp activeLamp remainingActiveLamps index
      else if index == activeLamp.index then activeLamp
      else if (Lamp.active lastLamp) then
        Lamp.interpolate lastLamp activeLamp index
      else
        Lamp (Selector.ColorSelection activeLamp.selector.color False Selector.Passive) index

setLamp : Strip -> Lamp -> Strip
setLamp strip lamp =
  if (Lamp.active lamp) then { strip | activeLamps = (setActiveLamp strip.activeLamps lamp) }
  else { strip | activeLamps = (setPassiveLamp strip.activeLamps lamp) }

setActiveLamp : List Lamp -> Lamp -> List Lamp
setActiveLamp lamps lamp =
  case lamps of
    [] -> [lamp]
    activeLamp :: remainingActiveLamps ->
      if lamp.index < activeLamp.index then lamp :: activeLamp :: remainingActiveLamps
      else if lamp.index == activeLamp.index then lamps
      else activeLamp :: (setActiveLamp remainingActiveLamps lamp)

setPassiveLamp : List Lamp -> Lamp -> List Lamp
setPassiveLamp lamps lamp =
  case lamps of
    [] -> lamps
    activeLamp :: remainingActiveLamps ->
      if activeLamp.index == lamp.index then remainingActiveLamps
      else activeLamp :: (setPassiveLamp remainingActiveLamps lamp)

type alias Config msg =
  { x1 : Float, y1 : Float
  , x2 : Float, y2 : Float
  , onLampClick : Char -> Lamp -> msg
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
    dh = (config.y2 - config.y1)/(toFloat strip.lampCount - 1.0)
    stripLenght = toFloat (strip.lampCount - 1)
    lx = config.x1 + dw*(toFloat index)
    ly = config.y1 + dh*(toFloat index)
  in
  Lamp.view { x = lx, y = ly, onClick = config.onLampClick strip.code } (getLamp strip index)
