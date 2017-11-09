module Puppetry.ColorSelect exposing (..)

import Color exposing (Color, rgb, toRgb, toHsl)
-- import Html exposing (Html)
-- import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
-- import Mouse exposing (position, Position, moves)

type alias Selection =
  { dist : Float
  , angle : Float
  }

type alias Model =
  { name : String
  , color : Color
  , selection : Maybe Selection
  , isOn : Bool
  , position : Position
  }

type alias Position =
    { x : Float
    , y : Float
    }


default : String -> Float -> Float -> Model
default name x y =
  { name = name
  , position = {x = x, y = y}
  , color = rgb 128 128 0
  , isOn = True
  , selection = Nothing
  }

type Msg
  = Move Position
  | SelectionDone
  | SelectionStart String


update_ : Msg -> Model -> Model
update_ msg model =
    case model.selection of
        Just sel ->
            case msg of
                Move pos ->
                    { model | selection = Just (selectionFromPosition pos model)}
                SelectionDone ->
                    if sel.dist < 20
                    then { model | isOn = not model.isOn, selection = Nothing}
                    else if sel.dist > 120
                    then { model | selection = Nothing}
                    else { model | color = colorFromSelection sel, selection = Nothing}
                SelectionStart name ->
                    model
        Nothing ->
            case msg of
                 SelectionStart name ->
                     if model.name == name
                     then { model | selection = Just ({dist = 0, angle = 0})}
                     else model
                 _ ->
                     model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (update_ msg model, Cmd.none)

selectionFromPosition : Position -> Model -> Selection
selectionFromPosition pos model =
    let vect =
            { x = pos.y - model.position.y
            , y = pos.x - model.position.x
            }
    in
        { angle = atan2 vect.x vect.y
        , dist = sqrt (vect.x^2 + vect.y^2)
        }


colorFromSelection : Selection -> Color
colorFromSelection sel =
    let clp = ((clamp 20 120 sel.dist) - 20) / 100
    in Color.hsl (radians sel.angle) 1 clp

selectionFromColor : Color -> Selection
selectionFromColor color =
    let hsl = toHsl color
        dist = (hsl.lightness) * 100 + 20
    in Selection dist (hsl.hue)


view : Model -> Svg Msg
view current =
  let color = toRgb current.color
  in g [ transform ("translate("
             ++ toString current.position.x ++ ","
             ++ toString current.position.y ++ ")")]
      ((case current.selection of
           Just sel ->
            let csel = selectionFromColor current.color
            in (drawSelection 1 csel) ++ (drawSelection 5 sel)
           Nothing -> []
      ) ++
       [ circle
        [ r "20"
        , onMouseDown (SelectionStart current.name)
        , strokeWidth "2px"
        , stroke "black"
        , fill (if (current.isOn) then (colorToCss color) else "black")
        ] []
       , circle
        [ r "7"
        , onMouseDown (SelectionStart current.name)
        , fill (colorToCss color)
        ] []
       ]
      )

toPx : Float -> String
toPx n = (toString n) ++ "px"

drawSelection : Float -> Selection -> List (Svg Msg)
drawSelection n sel =
  [ circle
      [ stroke (colorFromSelection sel |> toRgb |> colorToCss)
      , strokeWidth (toPx n)
      , fill "none"
      , r (toString sel.dist)
      ] []
  , line
      [ stroke (colorFromSelection sel |> toRgb |> colorToCss)
      , strokeWidth (toPx n)
      , x2 (toPx ((cos sel.angle) * sel.dist))
      , y2 (toPx ((sin sel.angle) * sel.dist))
      ] []
  ]

colorToCss : { d | blue : a, green : b, red : c } -> String
colorToCss color =
    "rgb(" ++ toString color.red ++ ","
           ++ toString color.green ++ ","
           ++ toString color.blue ++ ")"
