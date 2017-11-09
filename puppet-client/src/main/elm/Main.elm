--import Color exposing (Color, rgb, toRgb, toHsl)
import Html exposing (Html)
-- import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
--import Svg.Events exposing (..)
--import Mouse exposing (position, Position, moves)
-- import Html exposing (Html)
import Puppetry.ColorSelect as ColorSelect

type alias Model = ColorSelect.Model
type alias Msg = ColorSelect.Msg


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = testView
    , update = update
    , subscriptions = subscriptions
    }

init : ( Model, Cmd Msg )
init = ColorSelect.init

update
  : Msg
  -> Model
  -> ( Model, Cmd Msg )
update = ColorSelect.update

subscriptions : Model -> Sub Msg
subscriptions = ColorSelect.subscriptions

testView : Model -> Html Msg
testView m =
  svg [ width "400", height "400"]
    [ ColorSelect.view m ColorSelect.SetColor ]
