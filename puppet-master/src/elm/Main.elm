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
import Puppetry.Lamp as Lamp exposing (Lamp, lamp, lampColor)
import Puppetry.Strip as Strip exposing (Strip, getLamp)
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
  { selector : ColorSelection
  , strip : Strip
  , selectedLamp : Lamp
  , text : String
  , number : Int
  }

type Msg
  = Receive String
  | Send
  | SelectionChanged ColorSelection
  | LampClicked Lamp
  | Dummy

init : (Model, Cmd Msg)
init =
  let
    sLamp = lamp 0 1
  in
    ( { selector = ColorSelector.init (rgb 255 255 0)
      , strip = Strip 26 []
      , selectedLamp = sLamp
      , number = 0
      , text = "Position goes here"
      }
    , Cmd.none
    )

view : Model -> Html Msg
view model =
  div []
    [ svg [ viewBox "0 0 1000 700", width "1000px" ]
       [ Strip.view
          { x1 = 100.0, y1 = 100.0
          , x2 = 900.0, y2 = 100.0
          , onLampClick = LampClicked
          } model.strip
       , ColorSelector.view { x = 500, y = 450, onChange = SelectionChanged } model.selector
       ]
    , div []
       [ p [] [ Html.text <| "Pokes: " ++ toString model.number ]
       , button [ onClick Send ] [ Html.text "Poke others" ]
       ]
    , div [] [ Html.text model.text ]
    ]

-- lampView : Model -> Int -> Svg Msg
-- lampView model index =
--   let
--     lx = toFloat (100 + 400*index)
--   in
--     case (get index model.lamps) of
--       Just lamp -> Lamp.view { x = lx, y = 105, onClick = LampClicked } lamp
--       Nothing -> rect [ x (toString (lx - 3)), y "102", width "6", height "6", fill "red"] []

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
    SelectionChanged colorModel ->
      { model
      | selector = colorModel
      -- , lamps = (Array.set model.selectedLamp.lampIndex (lampColor colorModel model.selectedLamp) model.lamps)
      , text = (ColorSelector.toText colorModel.state)
      } ! []
    LampClicked lampModel ->
      { model
      | selector = lampModel.selector
      , selectedLamp = lampModel
      , text = ("Lamp #"++(toString lampModel.lampIndex)++" clicked")
      } ! []
    Dummy -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive
