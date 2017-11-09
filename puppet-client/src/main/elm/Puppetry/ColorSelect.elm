module Puppetry.ColorSelect exposing (..)

import Color exposing (Color, rgb, toRgb, toHsl)
import Html exposing (Html)
-- import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Mouse exposing (position, Position, moves)

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
        , Mouse.ups (\_ -> if x.dist < 20 then SetSelectionMode Nothing else SetColor (colorFromSelection x))
        ]


colorFromSelection : Selection -> Color
colorFromSelection sel =
    let clp = ((clamp 20 120 sel.dist) - 20) / 100
    in Color.hsl (radians sel.angle) 1 clp

selectionFromColor : Color -> Selection
selectionFromColor color =
    let hsl = toHsl color
        dist = (hsl.lightness) * 100 + 20
    in Selection dist (hsl.hue)

update : Msg -> Model -> (Model, Cmd Msg)
update color model =
  case color of
    SetColor color ->
      ( { model | color = color, selection = Nothing}, Cmd.none)
    SetSelectionMode b ->
      ( { model | selection = b }, Cmd.none)


view : Model -> (Color -> Msg) -> Svg Msg
view current f =
  let color = toRgb current.color
  in g [ transform ("translate("
             ++ toString current.position.x ++ ","
             ++ toString current.position.y ++ ")")]
      ((case current.selection of
           Just sel ->
            let csel = selectionFromColor current.color
            in [
                circle
                    [ stroke (colorFromSelection sel |> toRgb |> colorToCss)
                    , strokeWidth "5px"
                    , fill "none"
                    , r (toString sel.dist)
                    ] []
                , circle
                    [ stroke (current.color |> toRgb |> colorToCss)
                    , strokeWidth "2px"
                    , fill "none"
                    , r (toString csel.dist)
                    ] []
                , line
                    [ stroke (colorFromSelection sel |> toRgb |> colorToCss)
                    , strokeWidth "5px"
                    , x2 (toString ((cos sel.angle) * sel.dist) ++ "px")
                    , y2 (toString ((sin sel.angle) * sel.dist) ++ "px")
                    ] []
                , line
                    [ stroke (current.color |> toRgb |> colorToCss)
                    , strokeWidth "2px"
                    , x2 (toString ((cos csel.angle) * csel.dist) ++ "px")
                    , y2 (toString ((sin csel.angle) * csel.dist) ++ "px")
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

colorToCss : { d | blue : a, green : b, red : c } -> String
colorToCss color =
    "rgb(" ++ toString color.red ++ ","
           ++ toString color.green ++ ","
           ++ toString color.blue ++ ")"
