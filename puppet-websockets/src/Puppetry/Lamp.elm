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

type alias Model =
  { selector : ColorSelection
  , rackIndex : Int
  , lampIndex : Int
  }

lamp : Int -> Int -> Model
lamp r l =
  { selector = ColorSelector.init black
  , rackIndex = r
  , lampIndex = l
  }

lampColor : ColorSelection -> Model -> Model
lampColor cs model =
  { model | selector = cs }
  
type alias Config msg =
  { x : Int
  , y : Int
  , onClick : Model -> msg
  }

view : Config msg -> Model -> Svg msg
view config model =
  circle
    [ cx (toString config.x), cy (toString config.y), r (toString lampRadius)
    , strokeWidth "1"
    , stroke "blue"
    , fill (colorToCss model.selector.color)
    , SingleTouch.onEnd (handleClick config model)
    ] []

handleClick : Config msg -> Model -> Touch.Coordinates -> msg
handleClick config model coordinates =
   config.onClick model
