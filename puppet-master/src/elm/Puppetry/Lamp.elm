module Puppetry.Lamp exposing (..)

import Color exposing (Color, rgb, toRgb, toHsl, black)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Puppetry.ColorSelector as ColorSelector exposing (ColorSelection)
import Puppetry.Utilities exposing (..)
import Json.Encode as JE

import Touch
import SingleTouch

lampRadius : Float
lampRadius = 10

type alias Lamp =
  { selector : ColorSelection
  , index : Int
  }

jsValue : Lamp -> JE.Value
jsValue lamp =
  let
    { red, green, blue } = toRgb lamp.selector.color
  in
    JE.object
      [ ("lamp", JE.int lamp.index)
      , ("color", JE.object
        [ ("red", JE.int red)
        , ("green", JE.int green)
        , ("blue", JE.int blue)
        ] )
      ]

lamp : Int -> Lamp
lamp i =
  { selector = ColorSelector.init black
  , index = i
  }

activeLamp : Color -> Int -> Lamp
activeLamp color index =
  { selector = (ColorSelection color True ColorSelector.Passive)
  , index = index
  }

passiveLamp : Color -> Int -> Lamp
passiveLamp color index =
  { selector = (ColorSelection color False ColorSelector.Passive)
  , index = index
  }

lampColor : ColorSelection -> Lamp -> Lamp
lampColor cs model =
  { model | selector = cs }

type alias Config msg =
  { x : Float
  , y : Float
  , onClick : Lamp -> msg
  }

active : Lamp -> Bool
active lamp =
  lamp.selector.active

interpolate : Lamp -> Lamp -> Int -> Lamp
interpolate start end index =
  let
    startRgb = toRgb start.selector.color
    endRgb = toRgb end.selector.color
    length = end.index - start.index
    di = index - start.index
    dr = (endRgb.red - startRgb.red)//length
    dg = (endRgb.green - startRgb.green)//length
    db = (endRgb.blue - startRgb.blue)//length
    color = rgb (startRgb.red + di*dr) (startRgb.green + di*dg) (startRgb.blue + di*db)
  in
    Lamp (ColorSelector.init color) index

view : Config msg -> Lamp -> Svg msg
view config model =
  let
    (size, opacity, orbit) =
      if model.selector.active then (1.8, 0.8, "white")
      else (1.4, 0.5, "black")
  in
    g [ SingleTouch.onEnd (handleClick config model) ]
      [ circle
        [ cx (toString config.x), cy (toString config.y), r (toString (size*lampRadius))
        , fill (colorToCss model.selector.color)
        , fillOpacity (toString opacity)
        ] []
      , circle
        [ cx (toString config.x), cy (toString config.y), r (toString lampRadius)
        , strokeWidth "1"
        , stroke orbit
        , fill (colorToCss model.selector.color)
        ] []
      ]

handleClick : Config msg -> Lamp -> Touch.Coordinates -> msg
handleClick config model coordinates =
   config.onClick model
