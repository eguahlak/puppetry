module Puppetry.Utilities exposing (..)

import Color exposing (Color, fromRGB, toRGB, toHSL)
import String exposing (fromFloat)
import Json.Decode as JD exposing (Decoder)


type alias Position = { x : Float, y : Float }

type alias Selection = { dist : Float, angle : Float }

selectionFromPosition : Position -> Selection
selectionFromPosition { x, y } =
   Selection (sqrt (x^2 + y^2)) (atan2 y x)

colorToCss : Color -> String
colorToCss color =
  let (red, green, blue) = toRGB color
  in   "rgb(" ++ fromFloat red
       ++ "," ++ fromFloat green
       ++ "," ++ fromFloat blue
       ++ ")"

decodeColor : Decoder Color
decodeColor =
  JD.map3 (\r g b -> fromRGB (r, g, b))
    (JD.field "red" JD.float)
    (JD.field "green" JD.float)
    (JD.field "blue" JD.float)
