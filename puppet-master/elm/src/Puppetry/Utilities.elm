module Puppetry.Utilities exposing (..)

import Json.Decode as JD exposing (Decoder)
import String exposing (fromFloat)


type alias Position =
    { x : Float, y : Float }
