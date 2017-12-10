module Main exposing (main)

import Debug exposing (..)

-- import Html.App    as App
import Color exposing (Color, rgb)
import Html exposing (..)
import Json.Decode as JD exposing (Decoder, decodeString)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import WebSocket
-- import Puppetry.ColorSelect as ColorSelect
import Puppetry.ColorSelector as ColorSelector exposing (ColorSelection)
import Puppetry.Lamp as Lamp exposing (Lamp, lamp, lampColor, activeLamp, passiveLamp)
import Puppetry.Strip as Strip exposing (Strip, getLamp)

main : Program Never Model Msg
main =
  Html.program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

type alias Model =
  { selector : ColorSelection
  , lights : LightState
  , selectedStripCode : Maybe Char
  , selectedLampIndex : Int
  , number : Int
  , text : String
  }

type Msg
  = Receive String
  | Send
  | SelectionChanged ColorSelection
  | LampClicked Char Lamp
  | Dummy

type alias LightState =
  { backSceneStrip : Strip
  , middleSceneStrip : Strip
  , frontSceneStrip : Strip
  , leftStrip : Strip
  , rightStrip : Strip
  , proSceneStrip : Strip
  }

decodeLights : Decoder LightState
decodeLights =
  JD.map6 LightState
    (JD.field "back" (decodeStrip 'B' 26))
    (JD.field "middle" (decodeStrip 'M' 26))
    (JD.field "front" (decodeStrip 'F' 26))
    (JD.field "left" (decodeStrip 'L' 6))
    (JD.field "right" (decodeStrip 'R' 6))
    (JD.field "proscenium" (decodeStrip 'P' 23))

decodeStrip : Char -> Int -> Decoder Strip
decodeStrip c m =
  JD.map (\ l -> Strip c m l Nothing)
    decodeLamps

decodeLamps : Decoder (List Lamp)
decodeLamps =
  JD.list decodeLamp

decodeLamp : Decoder Lamp
decodeLamp =
  JD.map2 (\a b -> { selector = a, index = b})
    (JD.field "color" decodeColorSelection)
    (JD.field "lamp" JD.int )

decodeColorSelection : Decoder ColorSelection
decodeColorSelection =
  JD.map ColorSelector.init decodeColor

decodeColor : Decoder Color
decodeColor =
  JD.map3 rgb
    (JD.field "red" JD.int)
    (JD.field "green" JD.int)
    (JD.field "blue" JD.int)

init : (Model, Cmd Msg)
init =
  ( { selector = ColorSelector.init (rgb 255 255 0)
    , lights =
      { backSceneStrip = Strip 'B' 26 [] Nothing
      , middleSceneStrip = Strip 'M' 26 [] Nothing
      , frontSceneStrip = Strip 'F' 26 [] Nothing
      , proSceneStrip = Strip 'P' 23 [] Nothing
      , leftStrip = Strip 'L' 6 [] Nothing
      , rightStrip = Strip 'R' 6 [] Nothing
      }
    , selectedStripCode = Nothing
    , selectedLampIndex = 0
    , number = 0
    , text = "Debug info goes here"
    }
  , Cmd.none
  )

view : Model -> Html Msg
view model =
  div []
    [ svg [ viewBox "0 0 1000 700", width "1000px" ]
       [ Strip.view
           { x1 = 50.0, y1 = 100.0
           , x2 = 950.0, y2 = 100.0
           , onLampClick = LampClicked
           } model.lights.frontSceneStrip
       , Strip.view
           { x1 = 75.0, y1 = 150.0
           , x2 = 925.0, y2 = 150.0
           , onLampClick = LampClicked
           } model.lights.middleSceneStrip
       , Strip.view
           { x1 = 100.0, y1 = 200.0
           , x2 = 900.0, y2 = 200.0
           , onLampClick = LampClicked
           } model.lights.backSceneStrip
       , Strip.view
           { x1 = 125.0, y1 = 650.0
           , x2 = 875.0, y2 = 650.0
           , onLampClick = LampClicked
           } model.lights.proSceneStrip
       , Strip.view
           { x1 = 75.0, y1 = 550.0
           , x2 = 75.0, y2 = 350.0
           , onLampClick = LampClicked
           } model.lights.leftStrip
       , Strip.view
           { x1 = 925.0, y1 = 550.0
           , x2 = 925.0, y2 = 350.0
           , onLampClick = LampClicked
           } model.lights.rightStrip
       , ColorSelector.view { x = 500, y = 400, onChange = SelectionChanged } model.selector
       ]
    , div []
       [ p [] [ Html.text <| "Pokes: " ++ toString model.number ]
       , button [ onClick Send ] [ Html.text "Poke others" ]
       ]
    , div [] [ Html.text model.text ]
    ]

wsUrl : String
wsUrl = "ws://localhost:3000"
-- wsUrl = "ws://~:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Receive "poke" ->
      { model | number = model.number + 1}  ! []
    Receive json ->
      case decodeString decodeLights (log "received" json) of
        Ok lights ->
          { model | lights = lights } ! []
        Err msg ->
          { model | text = msg } ! []
    Send ->
      model ! [ WebSocket.send wsUrl "poke" ]
    SelectionChanged colorModel ->
      -- TODO: I think this is a hack
      -- DONE: Fixed it ;-)
      case model.selectedStripCode of
        Just c ->
          { model
          | selector = colorModel
          , lights = updateStrip c (\ s ->
               Strip.setLamp s (Lamp colorModel model.selectedLampIndex)
               ) model.lights
          } ! []
        _ -> model ! []
    LampClicked stripCode lamp ->
      { model
      | selector = lamp.selector
      , selectedStripCode = Just stripCode
      , selectedLampIndex = lamp.index
      } ! []
    Dummy -> (model, Cmd.none)

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
  WebSocket.listen wsUrl Receive
