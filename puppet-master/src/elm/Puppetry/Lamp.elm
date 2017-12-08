module Puppetry.Lamp exposing (..)

import Color exposing (Color, rgb, toRgb, toHsl, black)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Puppetry.ColorSelector as ColorSelector exposing (ColorSelection)
import Puppetry.Utilities exposing (..)

import Touch
import SingleTouch

lampRadius : Float
lampRadius = 10

type alias Lamp =
  { selector : ColorSelection
  , rackIndex : Int
  , lampIndex : Int
  }

lamp : Int -> Int -> Lamp
lamp r l =
  { selector = ColorSelector.init black
  , rackIndex = r
  , lampIndex = l
  }

lampColor : ColorSelection -> Lamp -> Lamp
lampColor cs model =
  { model | selector = cs }

type alias Config msg =
  { x : Float
  , y : Float
  , onClick : Lamp -> msg
  }

view : Config msg -> Lamp -> Svg msg
view config model =
  circle
    [ cx (toString config.x), cy (toString config.y), r (toString lampRadius)
    , strokeWidth "1"
    , stroke "blue"
    , fill (colorToCss model.selector.color)
    , SingleTouch.onEnd (handleClick config model)
    ] []

handleClick : Config msg -> Lamp -> Touch.Coordinates -> msg
handleClick config model coordinates =
   config.onClick model
