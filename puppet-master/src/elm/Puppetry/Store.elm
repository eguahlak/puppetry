module Puppetry.Store exposing (..)

import Color exposing (Color, rgb)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

buttonWidth: Float
buttonWidth = 20

buttonHeight: Float
buttonHeight = 20

-- Model

type alias Store =
  { color : Color
  , active : Bool
  , index : Int
  }

type alias StoreConfig = -- msg =
  { x : Float
  , y : Float
--  , onClickSave : Store -> msg
--  , onClickLoad : Store -> msg
  }

-- view : StoreConfig msg -> Store -> Svg msg
view : StoreConfig -> Store -> Svg msg
view config model =
  let
    frame = if model.active then "red" else "black"
  in
    g []
      [ rect
        [ x (toString (config.x - buttonWidth/2))
        , y (toString (config.y - buttonHeight))
        , width (toString buttonWidth)
        , height (toString buttonHeight)
        , strokeWidth "3"
        , stroke frame
        ]
        [ text (toString model.index ) 
        ]
      ]
