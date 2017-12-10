module Puppetry.Lamp exposing (..)

import Color exposing (Color, rgb, toRgb, toHsl, black)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (..)
import Puppetry.Utilities exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Touch
import SingleTouch

lampRadius : Float
lampRadius = 10

type alias Lamp =
  { color : Color
  , index : Int
  }

jsValue : Lamp -> JE.Value
jsValue lamp =
  let
    { red, green, blue } = toRgb lamp.color
  in
    JE.object
      [ ("lamp", JE.int lamp.index)
      , ("color", JE.object
        [ ("red", JE.int red)
        , ("green", JE.int green)
        , ("blue", JE.int blue)
        ] )
      ]

decode : Decoder Lamp
decode =
  JD.map2 Lamp
    (JD.field "color" decodeColor)
    (JD.field "lamp" JD.int )

decodeColor : Decoder Color
decodeColor =
  JD.map3 rgb
    (JD.field "red" JD.int)
    (JD.field "green" JD.int)
    (JD.field "blue" JD.int)


type alias StripLamp =
    { color : Color
    , active : Bool
    , index : Int
    }

add : Color -> Bool -> Int -> List StripLamp -> List StripLamp
add c a i xs = (StripLamp c a i) :: xs

-- Generate a list of StripLamps, requires the lamps to be in descending order
striplamps : List Lamp
           -> Maybe Lamp
           -> Int
           -> List StripLamp
           -> List StripLamp
striplamps lms lst idx st =
    if idx <= 0
    then
        st
    else
        case lms of
            lm :: rest ->
              if lm.index == (idx - 1)
              then
                  striplamps rest (Just lm) (idx - 1)
                      <| add lm.color True (idx - 1) st
              else
                  let color =
                          case lst of
                              Just lm2 ->
                                  interpolate lm lm2 idx
                              Nothing ->
                                  lm.color
                  in
                      striplamps lms lst (idx - 1)
                          <| add color False (idx - 1) st
            [] ->
                let color =
                        case lst of
                            Just lm ->
                                lm.color
                            Nothing ->
                                Color.black
                in
                    striplamps lms lst (idx - 1)
                        <| add color False (idx - 1) st

interpolate : Lamp -> Lamp -> Int -> Color
interpolate start end index =
  let
    startRgb = toRgb start.color
    endRgb = toRgb end.color
    length = end.index - start.index
    di = index - start.index
    dr = (endRgb.red - startRgb.red)//length
    dg = (endRgb.green - startRgb.green)//length
    db = (endRgb.blue - startRgb.blue)//length
  in
    rgb (startRgb.red + di*dr) (startRgb.green + di*dg) (startRgb.blue + di*db)



-- View

type alias Config msg =
  { x : Float
  , y : Float
  , selected : Bool
  , onClick : StripLamp -> msg
  }


view : Config msg -> StripLamp -> Svg msg
view config model =
  let
    (size, opacity, orbit) =
      if config.selected then
          (2.2, 0.8, if model.active then "white" else "black")
      else if model.active
           then (1.8, 0.8, "white")
           else (1.4, 0.5, "black")
  in
    g [ SingleTouch.onEnd (handleClick config model)
      , onClick (config.onClick model)
      ]
      [ circle
        [ cx (toString config.x), cy (toString config.y), r (toString (size*lampRadius))
        , fill (colorToCss model.color)
        , fillOpacity (toString opacity)
        ] []
      , circle
        [ cx (toString config.x), cy (toString config.y), r (toString lampRadius)
        , strokeWidth "1"
        , stroke orbit
        , fill (colorToCss model.color)
        ] []
      ]

handleClick : Config msg -> StripLamp -> Touch.Coordinates -> msg
handleClick config model coordinates =
   config.onClick model
