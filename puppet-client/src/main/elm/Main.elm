--import Color exposing (Color, rgb, toRgb, toHsl)
import Html exposing (..)
import Html.Attributes as HtmlA
-- import Json.Decode as Decode
import Task
import Svg exposing (..)
import Window
import Svg.Attributes as SvgA
--import Svg.Events exposing (..)
import Mouse exposing (position, Position, moves)
-- import Html exposing (Html)
import Puppetry.ColorSelect as ColorSelect
import List

import Touch
import SingleTouch

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
    , Task.attempt
           (Resize
                << Result.withDefault (Window.Size 400 400)
           )
           (Window.size)
    )

update
  : Msg
  -> Model
  -> ( Model, Cmd Msg )
update msg model =
    let x = case msg of
        ColorEvent e ->
            { model
                | colors =
                    List.map
                      (ColorSelect.update_ <| scaleMsg model e)
                       model.colors
                , message = (toString <| scaleMsg model e)
            }
        Resize e ->
            { model | windowSize = e, message = "Resized to: " ++ (toString e) }
        SetMessage m ->
            { model | message = m }
    in ( x , Cmd.none )


scaleMsg : Model -> ColorSelect.Msg -> ColorSelect.Msg
scaleMsg m msg =
    case msg of
        ColorSelect.Move p ->
            ColorSelect.Move (scalePos m p)
        ColorSelect.SelectionStart p ->
            ColorSelect.SelectionStart (scalePos m p)
        a ->
            a

scalePos : Model -> ColorSelect.Position -> ColorSelect.Position
scalePos m p =
    { x = (p.x - ((width m - (400 * scale m))/2)) / scale m
    , y = p.y / scale m
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves
            (\pos -> ColorEvent <| ColorSelect.Move
                 ({x = toFloat pos.x
                  , y = toFloat pos.y}
                 )
            )
        , Mouse.downs
            (\pos -> ColorEvent <| ColorSelect.SelectionStart
                 ({ x = toFloat pos.x
                  , y = toFloat pos.y
                  }))
        , Mouse.ups
            (\_ -> ColorEvent <| ColorSelect.SelectionDone )
        , Window.resizes (Resize)
        ]


scale : Model -> Float
scale m =
  Basics.min (width m / 400) (height m / 400)

height : Model -> Float
height m = toFloat m.windowSize.height - 40

width : Model -> Float
width m = toFloat m.windowSize.width

testView : Model -> Html Msg
testView model =
    div
      touchEvents
      [ svg
        [ SvgA.width (toString (400 * scale model))
        , SvgA.height (toString (400 * scale model))
        , HtmlA.style [("display", "block"), ("margin", "auto")]
        ]
        [ Svg.map ColorEvent <| g
          [ SvgA.transform ("scale(" ++ toString (scale model) ++ ")") ]
          (List.map (ColorSelect.view) model.colors)
        ]
      , div [ HtmlA.id "message"] [ Html.text <| model.message ]
      ]


posFromCoordinates : Touch.Coordinates -> ColorSelect.Position
posFromCoordinates a =
    let (x, y) = Touch.clientPos a in {x = x, y = y}

touchEvents : List (Html.Attribute Msg)
touchEvents =
    [ SingleTouch.onStart
          (\p ->  ColorEvent
               (ColorSelect.SelectionStart <| posFromCoordinates p))
    , SingleTouch.onMove
          (\p ->  ColorEvent (ColorSelect.Move <| posFromCoordinates p))
    , SingleTouch.onEnd ( \p -> ColorEvent (ColorSelect.SelectionDone))
    ]
