module Puppetry.Store exposing (..)

import Html.Events exposing (..)
import Html.Events.Extra.Touch as Touch
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Puppetry.Color exposing (..)
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
    { color : Maybe Color
    , index : Int
    }


type alias StoreConfig msg =
    { x : Float
    , y : Float
    , active : Bool
    , onClickSave : Store -> msg
    , onClickLoad : Store -> msg
    }


view : StoreConfig msg -> Store -> Svg msg
view config model =
    let
        circleColor =
            (case model.color of
                Just c ->
                    [ fill (colorToCss c) ]

                Nothing ->
                    [ fillOpacity "0" ]
            )
                ++ (if config.active then
                        [ strokeWidth "4"
                        , stroke "white"
                        , onClick (config.onClickSave model)
                        , Touch.onEnd (\a -> config.onClickSave model)
                        ]

                    else
                        [ strokeWidth "2"
                        , stroke "black"
                        , onClick (config.onClickLoad model)
                        , Touch.onEnd (\a -> config.onClickLoad model)
                        ]
                   )
    in
    g []
        [ circle
            ([ cx (fromFloat config.x)
             , cy (fromFloat config.y)
             , r "20"
             ]
                ++ circleColor
            )
            [ text (fromInt model.index)
            ]
        ]


handleClickSave : StoreConfig msg -> Store -> Touch.Event -> msg
handleClickSave config model _ =
    config.onClickSave model


handleClickLoad : StoreConfig msg -> Store -> Touch.Event -> msg
handleClickLoad config model _ =
    config.onClickLoad model
