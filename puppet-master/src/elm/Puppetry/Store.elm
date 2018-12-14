module Puppetry.Store exposing (...)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

-- Model

type alias Store =
  { color : Color
  , index : int
  }
