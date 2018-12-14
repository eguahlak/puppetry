module Puppetry.Utilities exposing (..)

import Color exposing (Color, rgb, toRgb, toHsl)
import Json.Decode as JD exposing (Decoder)


type alias Position = { x : Float, y : Float }

type alias Selection = { dist : Float, angle : Float }

selectionFromPosition : Position -> Selection
selectionFromPosition { x, y } =
   Selection (sqrt (x^2 + y^2)) (atan2 y x)

colorToCss : Color -> String
colorToCss color =
  let rgb = toRgb color
  in   "rgb(" ++ toString rgb.red
       ++ "," ++ toString rgb.green
       ++ "," ++ toString rgb.blue
       ++ ")"

decodeColor : Decoder Color
decodeColor =
  JD.map3 rgb
    (JD.field "red" JD.int)
    (JD.field "green" JD.int)
    (JD.field "blue" JD.int)
