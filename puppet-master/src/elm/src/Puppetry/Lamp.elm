module Puppetry.Lamp exposing (..)

import Color exposing (Color, fromRGB, toRGB, toHSL)
import String exposing (fromFloat)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (..)
import Puppetry.Utilities exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import Html.Events.Extra.Touch as Touch

lampRadius : Float
lampRadius = 10

type alias Lamp =
  { color : Color
  , index : Int
  }

jsValue : Lamp -> JE.Value
jsValue lamp =
  let
    ( red, green, blue) = toRGB lamp.color
  in
    JE.object
      [ ("lamp", JE.int lamp.index)
      , ("color", JE.object
        [ ("red", JE.float red)
        , ("green", JE.float green)
        , ("blue", JE.float blue)
        ] )
      ]

decode : Decoder Lamp
decode =
  JD.map2 Lamp
    (JD.field "color" decodeColor)
    (JD.field "lamp" JD.int )

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
                                Color.fromRGB (0, 0, 0)
                in
                    striplamps lms lst (idx - 1)
                        <| add color False (idx - 1) st

interpolate : Lamp -> Lamp -> Int -> Color
interpolate start end index =
  let
    (sr, sg, sb) = toRGB start.color
    (er, eg, eb) = toRGB end.color
    length = end.index - start.index
    di = toFloat (index - start.index)
    dr = (er - sr) / toFloat length
    dg = (eg - sg) / toFloat length
    db = (eb - sb) / toFloat length
  in
    fromRGB (sr + di*dr, sg + di*dg, sb + di*db)


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
    g [ Touch.onEnd (handleClick config model)
      , onClick (config.onClick model)
      ]
      [ circle
        [ cx (fromFloat config.x), cy (fromFloat config.y), r (fromFloat (size*lampRadius))
        , fill (colorToCss model.color)
        , fillOpacity (fromFloat opacity)
        ] []
      , circle
        [ cx (fromFloat config.x), cy (fromFloat config.y), r (fromFloat lampRadius)
        , strokeWidth "1"
        , stroke orbit
        , fill (colorToCss model.color)
        ] []
      ]

handleClick : Config msg -> StripLamp -> Touch.Event -> msg
handleClick config model _ =
   config.onClick model
