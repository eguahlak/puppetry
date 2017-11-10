--import Color exposing (Color, rgb, toRgb, toHsl)
import Html exposing (..)
import Html.Attributes as HtmlA
-- import Json.Decode as Decode
import Task
import Svg exposing (..)
import Window
import Svg.Attributes exposing (..)
--import Svg.Events exposing (..)
import Mouse exposing (position, Position, moves)
-- import Html exposing (Html)
import Puppetry.ColorSelect as ColorSelect
import List

-- import Touch
-- import SingleTouch

type alias Model =
    { windowSize : Window.Size
    , colors : List (ColorSelect.Model)
    , message : String
    }

type Msg
    = Resize Window.Size
    | ColorEvent ColorSelect.Msg
    | SetMessage String


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
     ( { windowSize = Window.Size 400 400
       , colors =
          [ ColorSelect.default "front" 100 100
          , ColorSelect.default "back" 300 300
          ]
       , message = "Hello, world!"
       }
    , Task.attempt (Resize << Result.withDefault (Window.Size 400 400)) (Window.size)
    )

update
  : Msg
  -> Model
  -> ( Model, Cmd Msg )
update msg model =
    let x = case msg of
        ColorEvent e ->
            { model
                | colors = List.map (ColorSelect.update_ e) model.colors
                , message = (toString e)
            }
        Resize e ->
            { model | windowSize = e, message = "Resized to: " ++ (toString e) }
        SetMessage m ->
            { model | message = m }
    in ( x , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves
            (\pos -> ColorEvent <| ColorSelect.Move
                 ({x = toFloat pos.x
                  , y = toFloat pos.y}
                 )
            )
        , Mouse.ups
            (\_ -> ColorEvent <| ColorSelect.SelectionDone )
        , Window.resizes (Resize)
        ]

testView : Model -> Html Msg
testView model =
    let scale =
            Basics.min
                (toFloat model.windowSize.width / 400)
                ((toFloat model.windowSize.height - 40) / 400)
    in div
    []
    [ svg
      [ width (toString (400 * scale))
      , height (toString <| model.windowSize.height - 40)
      , HtmlA.style [("display", "block"), ("margin", "auto")]
      ]
      [ Svg.map ColorEvent <| g
        [ transform ("scale(" ++ toString scale ++ ")") ]
        (List.map (ColorSelect.view) model.colors)
      ]
    , div [ id "message"] [ Html.text <| model.message ]
    ]
