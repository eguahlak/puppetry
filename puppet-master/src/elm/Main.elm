module Main exposing (main)

import Debug exposing (..)

-- import Html.App    as App
import Color exposing (Color, rgb)
import Html exposing (..)
import Navigation
import Json.Decode as JD exposing (Decoder, decodeString)
import Json.Encode as JE
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import WebSocket
import Puppetry.ColorSelector as ColorSelector exposing (ColorSelector)
import Puppetry.Lamp as Lamp exposing (Lamp)
import Puppetry.Strip as Strip exposing (Strip)

main : Program Never Model Msg
main =
  Navigation.program (SetLocation)
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

type alias Model =
  { selector : ColorSelector
  , lights : LightState
  , selectedStripCode : Maybe Char
  , selectedLampIndex : Int
  , number : Int
  , wsUrl : String
  , text : String
  }

type Msg
  = Receive String
  | SetLocation Navigation.Location
  | SetActiveLamp ColorSelector
  | SelectionChanged ColorSelector
  | LampClicked Char Lamp.StripLamp
  | Dummy

type alias LightState =
  { backSceneStrip : Strip
  , middleSceneStrip : Strip
  , frontSceneStrip : Strip
  , leftStrip : Strip
  , rightStrip : Strip
  , proSceneStrip : Strip
  }

jsValue : LightState -> JE.Value
jsValue lights =
  JE.object
    [ ("back", Strip.jsValue lights.backSceneStrip)
    , ("middle", Strip.jsValue lights.middleSceneStrip)
    , ("front", Strip.jsValue lights.frontSceneStrip)
    , ("left", Strip.jsValue lights.leftStrip)
    , ("right", Strip.jsValue lights.rightStrip)
    , ("proscenium", Strip.jsValue lights.proSceneStrip)
    ]

decodeLights : Decoder LightState
decodeLights =
  JD.map6 LightState
    (JD.field "back" (Strip.decode 'B' 26))
    (JD.field "middle" (Strip.decode 'M' 26))
    (JD.field "front" (Strip.decode 'F' 26))
    (JD.field "left" (Strip.decode 'L' 6))
    (JD.field "right" (Strip.decode 'R' 6))
    (JD.field "proscenium" (Strip.decode 'P' 23))

init : Navigation.Location -> (Model, Cmd Msg)
init l =
  ( { selector = ColorSelector.init (rgb 255 255 0) True
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
    , wsUrl = "ws://" ++ l.host
    , text = "Debug information here!"
    }
  , Cmd.none
  )

view : Model -> Html Msg
view model =
  let sel c = model.selectedStripCode
  |> Maybe.andThen (\cx -> if cx == c then Just model.selectedLampIndex else Nothing)
  in
  div []
    [ svg [ viewBox "0 0 1000 700", width "1000px" ]
       [ Strip.view
           { x1 = 50.0, y1 = 100.0
           , x2 = 950.0, y2 = 100.0
           , onLampClick = LampClicked
           , selected = sel 'F'
           } model.lights.frontSceneStrip
       , Strip.view
           { x1 = 75.0, y1 = 150.0
           , x2 = 925.0, y2 = 150.0
           , onLampClick = LampClicked
           , selected = sel 'M'
           } model.lights.middleSceneStrip
       , Strip.view
           { x1 = 100.0, y1 = 200.0
           , x2 = 900.0, y2 = 200.0
           , onLampClick = LampClicked
           , selected = sel 'B'
           } model.lights.backSceneStrip
       , Strip.view
           { x1 = 125.0, y1 = 650.0
           , x2 = 875.0, y2 = 650.0
           , onLampClick = LampClicked
           , selected = sel 'P'
           } model.lights.proSceneStrip
       , Strip.view
           { x1 = 75.0, y1 = 550.0
           , x2 = 75.0, y2 = 350.0
           , onLampClick = LampClicked
           , selected = sel 'L'
           } model.lights.leftStrip
       , Strip.view
           { x1 = 925.0, y1 = 550.0
           , x2 = 925.0, y2 = 350.0
           , onLampClick = LampClicked
           , selected = sel 'R'
           } model.lights.rightStrip
       , ColorSelector.view
           { x = 500
           , y = 400
           , onChange = SelectionChanged
           , onSelection = SetActiveLamp
           } model.selector
       ]
    , div [] [ Html.text model.text ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Receive json ->
      case decodeString decodeLights (log "received" json) of
        Ok lights ->
          { model | lights = lights } ! []
        Err msg ->
          { model | text = msg } ! []
    SelectionChanged sel ->
      { model | selector = sel } ! []
    SetActiveLamp sel ->
      case model.selectedStripCode of
        Just c ->
        let lights = updateStrip c (\ s ->
              if sel.active then
                Strip.setLamp s (Lamp sel.color model.selectedLampIndex)
              else
                Strip.removeLamp s model.selectedLampIndex
              ) model.lights
          in
          { model
          | selector = sel
          , lights = lights
          } ! [ WebSocket.send model.wsUrl (JE.encode 0 (jsValue lights))]
        _ -> model ! []
    LampClicked stripCode lamp ->
      { model
      | selector = ColorSelector.init lamp.color lamp.active
      , selectedStripCode = Just stripCode
      , selectedLampIndex = lamp.index
      } ! []
    Dummy -> (model, Cmd.none)
    SetLocation l ->
      { model | wsUrl = "ws://" ++ (log "Changed Location:" l).host} ! []


updateStrip : Char -> (Strip -> Strip) -> LightState -> LightState
updateStrip c fn l =
  case c of
    'B' -> { l | backSceneStrip = fn l.backSceneStrip }
    'M' -> { l | middleSceneStrip = fn l.middleSceneStrip }
    'F' -> { l | frontSceneStrip = fn l.frontSceneStrip }
    'P' -> { l | proSceneStrip = fn l.proSceneStrip }
    'L' -> { l | leftStrip = fn l.leftStrip }
    'R' -> { l | rightStrip = fn l.rightStrip }
    _ -> l


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen model.wsUrl Receive
