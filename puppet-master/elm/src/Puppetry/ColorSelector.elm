module Puppetry.ColorSelector exposing (..)

-- import Color exposing (Color, fromRGB, toRGB, toHSL)

import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Puppetry.Color exposing (..)
import Puppetry.Utilities exposing (..)
import String exposing (fromFloat, fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MODEL


type alias Model msg =
    { selection : ColorSelector
    , config : Config msg
    }


type alias ColorSelector =
    { color : Color
    , active : Bool
    , state : State
    }


init : Config msg -> Color -> Bool -> Model msg
init config c a =
    { config = config
    , selection =
        { color = c
        , active = a
        , state = Passive
        }
    }



-- UPDATE


type State
    = Passive
    | Setting Selection
    | Switching Selection



-- VIEW


buttonSize : Float
buttonSize =
    40


buttonReach : Float
buttonReach =
    120


type alias Config msg =
    { x : Float
    , y : Float
    , onSelection : ColorSelector -> msg
    }


inputMoved : Position -> Model msg -> Model msg
inputMoved pos model =
    { model
        | selection =
            modelChangeFromSelection
                (selectionFromPosition model.config pos)
                model.selection
    }


inputUp : Position -> Model msg -> ( Model msg, Bool )
inputUp pos model =
    let
        ( sel, x ) =
            modelEndFromSelection
                (selectionFromPosition model.config pos)
                model.selection
    in
    ( { model
        | selection = sel
      }
    , x
    )


setSelection : Color -> Bool -> Model msg -> Model msg
setSelection color bool model =
    { model
        | selection = ColorSelector color bool Passive
    }


type alias Selection =
    { dist : Float, angle : Float }


selectionFromPosition : Config msg -> Position -> Selection
selectionFromPosition config pos =
    let
        x =
            pos.x - config.x

        y =
            pos.y - config.y
    in
    Selection (sqrt (x ^ 2 + y ^ 2)) (atan2 y x)


view : Model msg -> Svg msg
view { config, selection } =
    g
        [ translate config ]
    <|
        List.concat
            [ viewColorCircle

            --, viewButton config selection
            , viewSelection selection
            ]


viewColorCircle : List (Svg msg)
viewColorCircle =
    let
        w =
            10

        h =
            5
    in
    List.concat <|
        List.map
            (\x ->
                List.map
                    (\i ->
                        viewColorCircleSquare
                            (toFloat x / h * buttonReach)
                            (buttonReach / h)
                            (toFloat i / w)
                            (1.0 / w)
                    )
                    (List.range 0 w)
            )
            (List.range 1 h)


viewColorCircleSquare : Float -> Float -> Float -> Float -> Svg msg
viewColorCircleSquare dist ddelta m delta =
    let
        dist_ =
            dist + buttonSize

        mkP deg l =
            { x = cos (deg * 2 * pi) * l
            , y = sin (deg * 2 * pi) * l
            }

        p1 =
            mkP (m - delta / 2) (dist_ - ddelta / 2)

        p2 =
            mkP (m + delta / 2) (dist_ - ddelta / 2)

        p3 =
            mkP (m - delta / 2) (dist_ + ddelta / 2)

        p4 =
            mkP (m + delta / 2) (dist_ + ddelta / 2)

        c =
            fromHSL ( m * 2 * pi, 1, Basics.min 1 (dist / buttonReach) )

        pA r d { x, y } =
            String.join " " [ "A", toPx d, toPx d, "0 0", r, toPx x, toPx y ]

        pM { x, y } =
            String.join " " [ "M", toPx x, toPx y ]

        pL { x, y } =
            String.join " " [ "L", toPx x, toPx y ]
    in
    Svg.path
        [ d <|
            String.join " "
                [ pM p1
                , pA "1" (dist_ - ddelta / 2) p2
                , pL p4
                , pA "0" (dist_ + ddelta / 2) p3
                , "Z"
                ]
        , fill (colorToCss c)
        ]
        []


viewButton : Config msg -> ColorSelector -> List (Svg msg)
viewButton config model =
    [ circle
        [ r (fromFloat buttonSize)
        , strokeWidth "2"
        , stroke "black"
        , fill
            (if model.active then
                colorToCss model.color

             else
                "black"
            )
        ]
        []
    , circle
        [ r (fromFloat (buttonSize / 2.5))
        , strokeWidth "0"
        , stroke "black"
        , fill (colorToCss model.color)
        ]
        []
    ]


viewSelection : ColorSelector -> List (Svg msg)
viewSelection { color, active, state } =
    case state of
        Passive ->
            []

        Switching _ ->
            [ circle
                [ r (fromFloat buttonSize)
                , strokeWidth "2"
                , stroke "black"
                , fill <|
                    if active then
                        "black"

                    else
                        colorToCss color
                ]
                []
            ]

        Setting selection ->
            [ circle
                [ r (fromFloat buttonSize)
                , strokeWidth "2"
                , stroke "black"
                , fill (colorToCss (colorFromSelection selection))
                ]
                []
            ]


toPx : Float -> String
toPx n =
    fromFloat n



-- UTILS


translate : Config msg -> Attribute msg
translate c =
    transform ("translate(" ++ fromFloat c.x ++ ", " ++ fromFloat c.y ++ ")")


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


stateOfSelection : Selection -> State
stateOfSelection selection =
    if selection.dist < buttonSize then
        Switching selection

    else if selection.dist < (buttonSize + buttonReach + 20) then
        Setting selection

    else
        Passive


modelChangeFromSelection : Selection -> ColorSelector -> ColorSelector
modelChangeFromSelection selection model =
    case stateOfSelection selection of
        Passive ->
            { model | state = Passive }

        Setting sel ->
            { model | state = Setting sel }

        Switching sel ->
            { model | state = Switching sel }


modelEndFromSelection : Selection -> ColorSelector -> ( ColorSelector, Bool )
modelEndFromSelection selection model =
    case stateOfSelection selection of
        Passive ->
            ( { model | state = Passive }, False )

        Setting sel ->
            ( { model | state = Passive, color = colorFromSelection sel }, True )

        Switching _ ->
            ( { model | state = Passive, active = not model.active }, True )


colorFromSelection : Selection -> Color
colorFromSelection { dist, angle } =
    fromHSL ( angle, 1, Basics.min 1 ((dist - buttonSize) / buttonReach) )
