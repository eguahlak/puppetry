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
import Puppetry.Lamp as Lamp exposing (lamp, lampColor)
import Array exposing (Array, fromList, get, set)

main : Program Never Model Msg
main =
  Html.program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

type alias Model =
  { left: ColorSelection
  , right: ColorSelection
  , lamps: Array Lamp.Model
  , selectedLamp: Lamp.Model
  , text: String
  , number: Int
  }

type Msg
  = Receive String
  | Send
  | LeftChanged ColorSelection
  | RightChanged ColorSelection
  | LampClicked Lamp.Model
  | Dummy

init : (Model, Cmd Msg)
init =
  let
    sLamp = lamp 0 1
  in
    ( { left = ColorSelector.init (rgb 255 255 0)
      , right = ColorSelector.init (rgb 255 0 255)
      , lamps = fromList [lamp 0 0, sLamp, lamp 0 2]
      , selectedLamp = sLamp
      , number = 0
      , text = "Position goes here"
      }
    , Cmd.none
    )

view : Model -> Html Msg
view model =
  div []
    [ svg [ viewBox "0 0 1000 500", width "1000px" ]
       [ rect
          [ x "100"
          , y "100"
          , width "800"
          , height "10"
          , fill "#0000ff"
          ] []
       , lampView model 0
       , lampView model 1
       , lampView model 2
       , ColorSelector.view { x = 200, y = 300, onChange = LeftChanged } model.left
       , ColorSelector.view { x = 800, y = 300, onChange = RightChanged } model.right
       ]
    , div []
       [ p [] [ Html.text <| "Pokes: " ++ toString model.number ]
       , button [ onClick Send ] [ Html.text "Poke others" ]
       ]
    , div [] [ Html.text model.text ]
    ]

lampView : Model -> Int -> Svg Msg
lampView model index =
  let
    lx = (100 + 400*index)
  in
    case (get index model.lamps) of
      Just lamp -> Lamp.view { x = lx, y = 105, onClick = LampClicked } lamp
      Nothing -> rect [ x (toString (lx - 3)), y "102", width "6", height "6", fill "red"] []

wsUrl : String
wsUrl = "ws://localhost:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Receive "poke" ->
      { model | number = model.number + 1}  ! []
    Receive _ ->
      model ! []
    Send ->
      model ! [ WebSocket.send wsUrl "poke" ]
    LeftChanged colorModel ->
      { model
      | left = colorModel
      , lamps = (Array.set model.selectedLamp.lampIndex (lampColor colorModel model.selectedLamp) model.lamps)
      , text = (ColorSelector.toText colorModel.state)
      } ! []
    RightChanged colorModel ->
      { model | right = colorModel, text = (ColorSelector.toText colorModel.state) } ! []
    LampClicked lampModel ->
      { model
      | left = lampModel.selector
      , selectedLamp = lampModel
      , text = ("Lamp #"++(toString lampModel.lampIndex)++" clicked")
      } ! []
    Dummy -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive
