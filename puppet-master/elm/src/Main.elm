port module Main exposing (main)

import Debug exposing (..)

-- import Html.App    as App
import Color exposing (Color, fromRGB)
import Html exposing (..)
import Browser

import Platform
import Json.Decode as JD exposing (Decoder, decodeString)
import Json.Encode as JE
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
-- import WebSocket
import Puppetry.ColorSelector as ColorSelector exposing (ColorSelector)
import Puppetry.Lamp as Lamp exposing (Lamp)
import Puppetry.Strip as Strip exposing (Strip)
import Puppetry.Store as Store exposing (Store)

main : Program () Model Msg
main =
  Browser.element
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

port websocketsIn  : (String -> msg) -> Sub msg
port websocketsOut : String -> Cmd msg

type alias Model =
  { selector : ColorSelector
  , lights : LightState
  , selectedStripCode : Maybe Char
  , selectedLampIndex : Int
  , number : Int
  , text : String
  }

type Msg
  = Receive String
  | SetActiveLamp ColorSelector
  | SelectionChanged ColorSelector
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

type alias PuppetryState =
  { lights: LightState
  }

updateStateTag : LightState -> JE.Value
updateStateTag lights =
  tagged "UpdateState" <|
    JE.object
      [ ("back", Strip.jsValue lights.backSceneStrip)
      , ("middle", Strip.jsValue lights.middleSceneStrip)
      , ("front", Strip.jsValue lights.frontSceneStrip)
      , ("left", Strip.jsValue lights.leftStrip)
      , ("right", Strip.jsValue lights.rightStrip)
      , ("proscenium", Strip.jsValue lights.proSceneStrip)
      ]

tagged : String -> JE.Value -> JE.Value
tagged name value =
  JE.object [ ("tag", JE.string name), ("contents", value)]

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

init : () -> (Model, Cmd Msg)
init _ =
  ( { selector = ColorSelector.init (fromRGB (255, 255, 0)) True
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
    }
  , Cmd.none
  )

view : Model -> Html Msg
view model =
  let sel c = model.selectedStripCode
        |> Maybe.andThen (\cx -> if cx == c then Just model.selectedLampIndex else Nothing)
  in
  div []
    [ svg
       [ viewBox "0 0 1000 900", width "1000px" ]
       (
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
         ] ++ (List.map (viewStore 10) (List.range 1 9))
       )
    , div [] [ Html.text model.text ]
    ]

viewStore: Int -> Int -> Svg Msg
viewStore l index=
  let
    (xValue, yValue) = (100.0 + toFloat(index*800)/toFloat(l), 730.0)
  in
    Store.view
       { x = xValue
       , y = yValue
       , onClickSave = \s -> SaveStore s.index
       , onClickLoad = \s -> LoadStore s.index
       } (Store (rgb 255 100 100) False index )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Receive json ->
      case decodeString decodePuppetry (log "received" json) of
        Ok puppetry ->
          ({ model | lights = puppetry.lights }, Cmd.none)
        Err err_msg ->
          ({ model | text = JD.errorToString err_msg }, Cmd.none)
    SelectionChanged sel ->
      ({ model | selector = sel }, Cmd.none)
    SetActiveLamp sel ->
      case model.selectedStripCode of
        Just c ->
          let lights = updateStrip c (\ s ->
                if sel.active
                then Strip.setLamp s (Lamp sel.color model.selectedLampIndex)
                else Strip.removeLamp s model.selectedLampIndex
                ) model.lights
          in
          ( { model
            | selector = sel
            , lights = lights
            }
          , websocketsOut (JE.encode 0 (updateStateTag lights))
          )
        _ -> ( model , Cmd.none)
    LampClicked stripCode lamp ->
        ( { model
          | selector = ColorSelector.init lamp.color lamp.active
          , selectedStripCode = Just stripCode
          , selectedLampIndex = lamp.index
          }
        , Cmd.none)
    SaveStore index ->
      (model, websocketsOut (JE.encode 0 (saveTag index)))
    LoadStore index ->
      (model, websocketsOut (JE.encode 0 (loadTag index)))
    _ -> (model, Cmd.none)


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
  websocketsIn Receive

rgb r g b = fromRGB (r, g, b)
