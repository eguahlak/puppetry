module Puppetry.Store exposing (..)

import Color exposing (Color, fromRGB)
import Html.Events exposing (..)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import String exposing (fromFloat, fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)


buttonWidth : Float
buttonWidth =
    40


buttonHeight : Float
buttonHeight =
    40



-- Model


type alias Store =
    { color : Color
    , active : Bool
    , index : Int
    }


type alias StoreConfig msg =
    { x : Float
    , y : Float
    , onClickSave : Store -> msg
    , onClickLoad : Store -> msg
    }


view : StoreConfig msg -> Store -> Svg msg
view config model =
    let
        frame =
            if model.active then
                "red"

            else
                "blue"
    in
    g []
        [ rect
            [ x (fromFloat (config.x - buttonWidth / 2))
            , y (fromFloat (config.y - buttonHeight))
            , width (fromFloat buttonWidth)
            , height (fromFloat buttonHeight)
            , strokeWidth "3"
            , stroke "black"
            , fill "green"
            , onClick (config.onClickLoad model)
            ]
            [ text "L"
            ]
        , rect
            [ x (fromFloat (config.x - buttonWidth / 2))
            , y (fromFloat (config.y + 30))
            , width (fromFloat buttonWidth)
            , height (fromFloat buttonHeight)
            , strokeWidth "3"
            , stroke "black"
            , fill "red"
            , onClick (config.onClickSave model)
            ]
            [ text "S"
            ]
        ]
