port module Main exposing (main)

-- import Html.App    as App

import Browser
import Color exposing (Color, fromRGB)
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes as HA exposing (style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as JD exposing (Decoder, decodeString)
import Json.Encode as JE
import Platform
import Puppetry.ColorSelector as ColorSelector exposing (ColorSelector)
import Puppetry.Lamp as Lamp exposing (Lamp)
import Puppetry.Store as Store exposing (Store)
import Puppetry.Strip as Strip exposing (Strip)
import Puppetry.Utilities exposing (..)
import String exposing (fromFloat, fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program WindowSize Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port websocketsIn : (String -> msg) -> Sub msg


port websocketsOut : String -> Cmd msg


port resize : ({ height : Int, width : Int } -> msg) -> Sub msg


type alias Model =
    { selector : ColorSelector
    , lights : LightState
    , selectedStripCode : Maybe Char
    , selectedLampIndex : Int
    , number : Int
    , text : String
    , windowSize : WindowSize
    , mousePos : Position
    }


type Msg
    = Receive String
    | Resize WindowSize
    | SetActiveLamp ColorSelector
    | SelectionChanged ColorSelector
    | SetPosition Position
    | LampClicked Char Lamp.StripLamp
    | SaveStore Int
    | LoadStore Int
    | Dummy


type alias LightState =
    { backSceneStrip : Strip
    , middleSceneStrip : Strip
    , frontSceneStrip : Strip
    , leftStrip : Strip
    , rightStrip : Strip
    , proSceneStrip : Strip
    }


type alias WindowSize =
    { height : Int
    , width : Int
    }


type alias PuppetryState =
    { lights : LightState
    }


updateStateTag : LightState -> JE.Value
updateStateTag lights =
    tagged "UpdateState" <|
        JE.object
            [ ( "back", Strip.jsValue lights.backSceneStrip )
            , ( "middle", Strip.jsValue lights.middleSceneStrip )
            , ( "front", Strip.jsValue lights.frontSceneStrip )
            , ( "left", Strip.jsValue lights.leftStrip )
            , ( "right", Strip.jsValue lights.rightStrip )
            , ( "proscenium", Strip.jsValue lights.proSceneStrip )
            ]


tagged : String -> JE.Value -> JE.Value
tagged name value =
    JE.object [ ( "tag", JE.string name ), ( "contents", value ) ]


loadTag : Int -> JE.Value
loadTag index =
    tagged "Load" (JE.int index)


saveTag : Int -> JE.Value
saveTag index =
    tagged "Save" (JE.int index)


decodeLights : Decoder LightState
decodeLights =
    JD.map6 LightState
        (JD.field "back" (Strip.decode 'B' 26))
        (JD.field "middle" (Strip.decode 'M' 26))
        (JD.field "front" (Strip.decode 'F' 26))
        (JD.field "left" (Strip.decode 'L' 6))
        (JD.field "right" (Strip.decode 'R' 6))
        (JD.field "proscenium" (Strip.decode 'P' 23))


decodePuppetry : Decoder PuppetryState
decodePuppetry =
    JD.map PuppetryState (JD.field "lights" decodeLights)


init : WindowSize -> ( Model, Cmd Msg )
init size =
    ( { selector = ColorSelector.init (fromRGB ( 255, 255, 0 )) True
      , lights =
            { backSceneStrip = Strip 'B' 26 []
            , middleSceneStrip = Strip 'M' 26 []
            , frontSceneStrip = Strip 'F' 26 []
            , proSceneStrip = Strip 'P' 23 []
            , leftStrip = Strip 'L' 6 []
            , rightStrip = Strip 'R' 6 []
            }
      , selectedStripCode = Nothing
      , selectedLampIndex = 0
      , number = 0
      , text = "Debug information here!"
      , windowSize = size
      , mousePos = { x = 0, y = 0 }
      }
    , Cmd.none
    )


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


view : Model -> Html Msg
view model =
    let
        sel c =
            model.selectedStripCode
                |> Maybe.andThen
                    (\cx ->
                        if cx == c then
                            Just model.selectedLampIndex

                        else
                            Nothing
                    )
    in
    div [ HA.style "text-align" "center" ]
        [ svg
            ([ viewBox "0 0 1000 900"
             , width (fromInt model.windowSize.width ++ "px")
             , Mouse.onMove
                (\m ->
                    let
                        ( x, y ) =
                            m.clientPos
                    in
                    SetPosition
                        { x = x / (toFloat model.windowSize.width / 1000)
                        , y = y / (toFloat model.windowSize.width / 1000)
                        }
                )
             , Touch.onMove
                (\m ->
                    let
                        ( x, y ) =
                            touchCoordinates m
                    in
                    SetPosition
                        { x = x / (toFloat model.windowSize.width / 1000)
                        , y = y / (toFloat model.windowSize.width / 1000)
                        }
                )
             ]
             -- ++ if model.windowSize.width <= model.windowSize.height
             --then [ ]
             -- else [ height (fromInt model.windowSize.height ++ "px") ]
            )
            ([ circle
                [ cx (fromFloat model.mousePos.x)
                , cy (fromFloat model.mousePos.y)
                , r "20"

                -- , fill
                -- , fillOpacity (fromFloat opacity)
                ]
                []
             , Strip.view
                { x1 = 50.0
                , y1 = 100.0
                , x2 = 950.0
                , y2 = 100.0
                , onLampClick = LampClicked
                , selected = sel 'F'
                }
                model.lights.frontSceneStrip
             , Strip.view
                { x1 = 75.0
                , y1 = 150.0
                , x2 = 925.0
                , y2 = 150.0
                , onLampClick = LampClicked
                , selected = sel 'M'
                }
                model.lights.middleSceneStrip
             , Strip.view
                { x1 = 100.0
                , y1 = 200.0
                , x2 = 900.0
                , y2 = 200.0
                , onLampClick = LampClicked
                , selected = sel 'B'
                }
                model.lights.backSceneStrip
             , Strip.view
                { x1 = 125.0
                , y1 = 650.0
                , x2 = 875.0
                , y2 = 650.0
                , onLampClick = LampClicked
                , selected = sel 'P'
                }
                model.lights.proSceneStrip
             , Strip.view
                { x1 = 75.0
                , y1 = 550.0
                , x2 = 75.0
                , y2 = 350.0
                , onLampClick = LampClicked
                , selected = sel 'L'
                }
                model.lights.leftStrip
             , Strip.view
                { x1 = 925.0
                , y1 = 550.0
                , x2 = 925.0
                , y2 = 350.0
                , onLampClick = LampClicked
                , selected = sel 'R'
                }
                model.lights.rightStrip
             , ColorSelector.view
                { x = 500
                , y = 400
                , onChange = SelectionChanged
                , onSelection = SetActiveLamp
                }
                model.selector
             ]
                ++ List.map (viewStore 10) (List.range 1 9)
            )
        ]


viewStore : Int -> Int -> Svg Msg
viewStore l index =
    let
        ( xValue, yValue ) =
            ( 100.0 + toFloat (index * 800) / toFloat l, 730.0 )
    in
    Store.view
        { x = xValue
        , y = yValue
        , onClickSave = \s -> SaveStore s.index
        , onClickLoad = \s -> LoadStore s.index
        }
        (Store (rgb 255 100 100) False index)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Receive json ->
            case decodeString decodePuppetry (log "received" json) of
                Ok puppetry ->
                    ( { model | lights = puppetry.lights }, Cmd.none )

                Err err_msg ->
                    ( { model | text = JD.errorToString err_msg }, Cmd.none )

        Resize size ->
            ( { model | windowSize = size }, Cmd.none )

        SetPosition pos ->
            ( { model | mousePos = pos }, Cmd.none )
{--
        MovePosition pos ->
            ( { model
              | selector = (ColorSelector.move pos)
              }
            , Cmd.none
            )

        UpPosition pos ->
            let
              (sel, action) = ColorSelector.up pos
            in
            ( { model
              | selector = sel)
              }
            , Cmd SetActiveLamp sel
            )
--}
        SelectionChanged sel ->
            ( { model | selector = sel }, Cmd.none )

        SetActiveLamp sel ->
            case model.selectedStripCode of
                Just c ->
                    let
                        lights =
                            updateStrip c
                                (\s ->
                                    if sel.active then
                                        Strip.setLamp s (Lamp sel.color model.selectedLampIndex)

                                    else
                                        Strip.removeLamp s model.selectedLampIndex
                                )
                                model.lights
                    in
                    ( { model
                        | selector = sel
                        , lights = lights
                      }
                    , websocketsOut (JE.encode 0 (updateStateTag lights))
                    )

                _ ->
                    ( model, Cmd.none )

        LampClicked stripCode lamp ->
            ( { model
                | selector = ColorSelector.init lamp.color lamp.active
                , selectedStripCode = Just stripCode
                , selectedLampIndex = lamp.index
              }
            , Cmd.none
            )

        SaveStore index ->
            ( model, websocketsOut (JE.encode 0 (saveTag index)) )

        LoadStore index ->
            ( model, websocketsOut (JE.encode 0 (loadTag index)) )

        _ ->
            ( model, Cmd.none )


updateStrip : Char -> (Strip -> Strip) -> LightState -> LightState
updateStrip c fn l =
    case c of
        'B' ->
            { l | backSceneStrip = fn l.backSceneStrip }

        'M' ->
            { l | middleSceneStrip = fn l.middleSceneStrip }

        'F' ->
            { l | frontSceneStrip = fn l.frontSceneStrip }

        'P' ->
            { l | proSceneStrip = fn l.proSceneStrip }

        'L' ->
            { l | leftStrip = fn l.leftStrip }

        'R' ->
            { l | rightStrip = fn l.rightStrip }

        _ ->
            l


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ websocketsIn Receive
        , resize Resize
        ]


rgb r g b =
    fromRGB ( r, g, b )
