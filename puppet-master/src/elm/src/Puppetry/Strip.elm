module Puppetry.Strip exposing (..)

import Debug exposing (log)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)
import Puppetry.Lamp as Lamp exposing (Lamp)

-- MODEL

type alias Strip =
  { code : Char
  , lampCount : Int
  , activeLamps : List Lamp
  }

jsValue : Strip -> JE.Value
jsValue strip =
  JE.list (List.map Lamp.jsValue strip.activeLamps)

decode : Char -> Int -> Decoder Strip
decode c i =
  JD.map ( Strip c i ) (JD.list Lamp.decode)


insert : Lamp -> List Lamp -> List Lamp
insert lm lms =
    case lms of
        lm2 :: rest ->
            if lm.index < lm2.index then
                lm :: lms
            else if lm.index == lm2.index then
                lm :: rest
            else
                lm2 :: insert lm rest
        [] ->
            lm :: []

setLamp : Strip -> Lamp -> Strip
setLamp strip lamp =
    { strip | activeLamps = insert lamp strip.activeLamps }

removeLamp : Strip -> Int -> Strip
removeLamp strip id =
    { strip | activeLamps = List.filter (\a -> a.index /= id) strip.activeLamps }

striplampsFromStrip : Strip -> List Lamp.StripLamp
striplampsFromStrip s =
    Lamp.striplamps (List.reverse s.activeLamps) Nothing s.lampCount []


-- VIEW

type alias Config msg =
  { x1 : Float, y1 : Float
  , x2 : Float, y2 : Float
  , selected : Maybe Int
  , onLampClick : Char -> Lamp.StripLamp -> msg
  }


view : Config msg -> Strip -> Svg msg
view config strip =
--    let lamps = log "StripLamps" <| striplampsFromStrip <| log "Strip" strip
    let lamps = striplampsFromStrip strip
    in g [] <|
        ( line
               [ x1 (toString config.x1)
               , y1 (toString config.y1)
               , x2 (toString config.x2)
               , y2 (toString config.y2)
               , strokeWidth "3"
               , stroke "black"
               ] []
         ) :: List.map (viewStripLamp config strip) lamps

viewStripLamp : Config msg -> Strip -> Lamp.StripLamp -> Svg msg
viewStripLamp config strip lamp =
  let
    dw = (config.x2 - config.x1)/(toFloat strip.lampCount - 1.0)
    dh = (config.y2 - config.y1)/(toFloat strip.lampCount - 1.0)
    stripLenght = toFloat (strip.lampCount - 1)
  in
    Lamp.view
        { x = config.x1 + dw*(toFloat lamp.index)
        , y = config.y1 + dh*(toFloat lamp.index)
        , onClick = config.onLampClick strip.code
        , selected =
            case config.selected of
                Just idx -> idx == lamp.index
                Nothing -> False
        } lamp
