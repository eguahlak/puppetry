module Main exposing (main)

-- import Html.App    as App
import Color exposing (rgb)
import Html        exposing (..)
import Html.Events exposing (..)
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
  , backSceneStrip : Strip
  , middleSceneStrip : Strip
  , number : Int
  }

type Msg
  = Receive String
  | Send
  | SelectionChanged ColorSelection
  | LampClicked Char Lamp
  | Dummy

init : (Model, Cmd Msg)
init =
  ( { selector = ColorSelector.init (rgb 255 255 0)
    , backSceneStrip = Strip 'B' 26 [(activeLamp Color.red 3), (activeLamp Color.blue 20)] Nothing
    , middleSceneStrip = Strip 'M' 26 [(activeLamp Color.green 13)] Nothing
    , number = 0
    }
  , Cmd.none
  )

view : Model -> Html Msg
view model =
  div []
    [ svg [ viewBox "0 0 1000 700", width "1000px" ]
       [ Strip.view
           { x1 = 75.0, y1 = 150.0
           , x2 = 925.0, y2 = 150.0
           , onLampClick = LampClicked
           } model.middleSceneStrip
       , Strip.view
           { x1 = 100.0, y1 = 200.0
           , x2 = 900.0, y2 = 200.0
           , onLampClick = LampClicked
           } model.backSceneStrip
       , ColorSelector.view { x = 500, y = 450, onChange = SelectionChanged } model.selector
       ]
       --    , div []
       --       [ p [] [ Html.text <| "Pokes: " ++ toString model.number ]
       --       , button [ onClick Send ] [ Html.text "Poke others" ]
       --       ]
       --    , div [] [ Html.text model.text ]
    ]

wsUrl : String
-- wsUrl = "ws://localhost:3000"
wsUrl = ""

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Receive "poke" ->
      { model | number = model.number + 1}  ! []
    Receive _ ->
      model ! []
    Send ->
      model ! [ WebSocket.send wsUrl "poke" ]
    SelectionChanged colorModel ->
      { model
      | selector = colorModel
      } ! []
    LampClicked stripCode lamp ->
      -- TODO: This is a hack i think (AK)
      case stripCode of
        'B' -> { model | selector = lamp.selector, backSceneStrip = Strip.setLamp model.backSceneStrip lamp } ! []
        'M' -> { model | selector = lamp.selector, middleSceneStrip = Strip.setLamp model.middleSceneStrip lamp } ! []
        _ -> model ! []
    Dummy -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive
