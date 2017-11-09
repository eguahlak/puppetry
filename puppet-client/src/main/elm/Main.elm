--import Color exposing (Color, rgb, toRgb, toHsl)
import Html exposing (..)
-- import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
--import Svg.Events exposing (..)
import Mouse exposing (position, Position, moves)
-- import Html exposing (Html)
import Puppetry.ColorSelect as ColorSelect
import List

-- import Touch
-- import SingleTouch

type alias Model = List (ColorSelect.Model)
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
init =
    ([ ColorSelect.default "front" 100 100
     , ColorSelect.default "back" 300 300
     ]
    , Cmd.none
    )

update
  : Msg
  -> Model
  -> ( Model, Cmd Msg )
update msg ms =
    (List.map (ColorSelect.update_ msg ) ms, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves
            (\pos -> ColorSelect.Move ({x = toFloat pos.x, y = toFloat pos.y}))
        , Mouse.ups
            (\_ -> ColorSelect.SelectionDone )
        ]

testView : Model -> Html Msg
testView ms =
    div
    []
    [ svg
      [ width "400", height "400"]
      (List.map (ColorSelect.view) ms)
    ]
