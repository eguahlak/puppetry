module Puppetry.Utilities exposing (..)

import String exposing (fromFloat)
import Json.Decode as JD exposing (Decoder)


type alias Position = { x : Float, y : Float }

type alias Selection = { dist : Float, angle : Float }

selectionFromPosition : Position -> Selection
selectionFromPosition { x, y } =
   Selection (sqrt (x^2 + y^2)) (atan2 y x)

