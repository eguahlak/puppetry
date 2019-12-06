module Puppetry.Window exposing (..)

{-| The goal of this module is to create a centered svg window which suscribes
to touch events
-}

import Html exposing (..)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import String exposing (fromFloat, fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Square =
    { width : Float, height : Float }


type alias WindowSize =
    Square


type alias Position =
    { x : Float, y : Float }


type alias Model msg =
    { -- The size of the window
      window : WindowSize

    -- The internal scale to use
    , scale : Square

    -- Event to fire if the mouse or touch were moved.
    , onInputMoved : Position -> msg

    -- Event to fire if the mouse or touch were selected, touch up, or mouse clicked.
    , onInputSelected : Position -> msg
    }


{-| Given a window model and a list of svg items render it into html.
-}
view : Model msg -> List (Svg msg) -> Html msg
view model =
    svg
        [ viewBox ("0 0 " ++ fromFloat model.scale.width ++ " " ++ fromFloat model.scale.height)
        , width (fromFloat model.window.width ++ "px")
        , Mouse.onMove (\m -> model.onInputMoved <| calculatePosition model m.clientPos)
        , Mouse.onClick (\m -> model.onInputSelected <| calculatePosition model m.clientPos)
        , Touch.onMove (\m -> model.onInputMoved <| calculatePosition model (touchCoordinates m))
        , Touch.onStart (\m -> model.onInputMoved <| calculatePosition model (touchCoordinates m))
        , Touch.onEnd (\m -> model.onInputSelected <| calculatePosition model (touchCoordinates m))
        ]



-- Updates


resize : WindowSize -> Model msg -> Model msg
resize size m =
    { m | window = size }



-- Utils


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


calculatePosition : Model msg -> ( Float, Float ) -> Position
calculatePosition { window, scale } ( x, y ) =
    if window.width * scale.height >= window.height * scale.width then
        let
            scalar =
                scale.width / window.width
        in
        { x = x * scalar
        , y = y * scalar
        }

    else
        { x = x
        , y = y
        }
