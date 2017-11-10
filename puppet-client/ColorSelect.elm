module ColorSelect exposing (..)

import Color exposing (Color, rgb, toRgb)
import Html exposing (Html)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Mouse exposing (position, Position, moves)


main =
  Html.program
    { init = init
    , view = testView
    , update = update
    , subscriptions = subscriptions
    }

type alias Selection =
  { dist : Float, angle : Float}

type alias Model =
  { color : Color
  , selection : Maybe Selection
  , position : Position
  }

init : (Model, Cmd Msg)
init =
    ( { position = { x = 200, y = 200 }
      , color = rgb 128 128 0
      , selection = Nothing
      }
    , Cmd.none )

type Msg
  = SetColor Color
  | SetSelectionMode (Maybe Selection)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.selection of
    Nothing -> Sub.none
    Just x ->
      Sub.batch
        [ Mouse.moves
              (\pos ->
                   let vect = { x = toFloat (pos.y - model.position.y)
                              , y = toFloat (pos.x - model.position.x)
                              }
                       angle = atan2 vect.x vect.y
                   in SetSelectionMode (Just { angle = atan2 vect.x vect.y
                                             , dist = sqrt (vect.x^2 + vect.y^2)
                                             })
              )
        , Mouse.ups (\_ -> SetColor (colorFromSelection x))
        ]


colorFromSelection sel =
    let clp = ((clamp 20 120 sel.dist) - 20) / 100
    in Color.hsl (radians sel.angle) 1 clp

update : Msg -> Model -> (Model, Cmd Msg)
update color model =
  case color of
    SetColor color ->
      ( { model | color = color, selection = Nothing}, Cmd.none)
    SetSelectionMode b ->
      ( { model | selection = b }, Cmd.none)


testView : Model -> Html Msg
testView m =
  svg [ width "400", height "400"]
    [ view m SetColor ]


view : Model -> (Color -> Msg) -> Svg Msg
view current f =
  let color = toRgb current.color
  in g [ transform ("translate("
             ++ toString current.position.x ++ ","
             ++ toString current.position.y ++ ")")]
      ((case current.selection of
           Just sel -> [
                circle
                    [ stroke (colorFromSelection sel |> toRgb |> colorToCss)
                    , strokeWidth "5px"
                    , fill "none"
                    , r (toString sel.dist)
                    ] []
           ]
           Nothing -> []
      ) ++
       [ circle
        [ r "20"
        , onMouseDown (SetSelectionMode (Just { dist = 0, angle = 0}))
        , stroke "black"
        , fill (colorToCss color)
        ] []
       ]
      )

colorToCss color =
    "rgb(" ++ toString color.red ++ ","
           ++ toString color.green ++ ","
           ++ toString color.blue ++ ")"
