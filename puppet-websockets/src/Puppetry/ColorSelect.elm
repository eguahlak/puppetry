module Puppetry.ColorSelect exposing (..)

import Color exposing (Color, rgb, toRgb, toHsl)
import Svg exposing (..)
import Svg.Attributes exposing (..)

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

buttonSize : Float
buttonSize = 20

buttonReach : Float
buttonReach = 100


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
  | SelectionStart Position


update_ : Msg -> Model -> Model
update_ msg model =
  case model.selection of
    Just sel ->
      case msg of
        Move pos ->
          { model
            | selection = Just (selectionFromPosition pos model)
          }
        SelectionDone ->
          case readSelection sel of
            Abort ->
              { model
                | selection = Nothing
              }
            Switch ->
              { model
                | isOn = not model.isOn
                , selection = Nothing
              }
            SetColor col ->
              { model
                | color = col
                , selection = Nothing
              }
        _ -> model
    Nothing ->
      case msg of
        SelectionStart pos ->
          let sel = (selectionFromPosition pos model)
          in if sel.dist < buttonSize then
            { model | selection = Just sel }
          else model
        _ ->
          model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (update_ msg model, Cmd.none)

selectionFromPosition : Position -> Model -> Selection
selectionFromPosition pos model =
    let x = pos.y - model.position.y
        y = pos.x - model.position.x
    in { angle = atan2 x y
       , dist = sqrt (x^2 + y^2)
       }

type Action
    = SetColor Color
    | Switch
    | Abort

readSelection : Selection -> Action
readSelection sel =
    if sel.dist < buttonSize then
      Switch
    else if sel.dist > buttonReach + buttonSize then
      Abort
    else
      SetColor <| colorFromAD sel.angle sel.dist

colorFromAD : Float -> Float -> Color
colorFromAD angle distance =
    Color.hsl angle 1 ((distance - buttonSize) / buttonReach)

selectionFromColor : Color -> Selection
selectionFromColor color =
    let hsl = toHsl color
        dist = (hsl.lightness) * 100 + 20
    in Selection dist (hsl.hue)


view : Model -> Svg Msg
view m =
  let
    button =
      [ circle
        [ r (toString buttonSize)
        , strokeWidth "2"
        , stroke "black"
        , fill (if (m.isOn) then (colorToCss m.color) else "black")
        ] []
      ]
    indicator =
      if not m.isOn then
        [ circle
          [ r (toString (buttonSize / 3))
          , strokeWidth "2"
          , stroke "black"
          , fill (colorToCss m.color)
          ] []
        ]
      else []
  in g [ transform ("translate("
             ++ toString m.position.x ++ ","
             ++ toString m.position.y ++ ")")
       ] <|
       List.concat
         [ case m.selection of
             Just sel ->
               viewSelection m sel
             Nothing ->
               []
         , button
         , indicator
         ]

viewSelection : Model -> Selection -> List (Svg msg)
viewSelection m sel =
  case readSelection sel of
    Abort -> []
    Switch ->
      [ circle
        [ r (toString (buttonSize * 1.50))
        , strokeWidth "2"
        , stroke "black"
        , fill <| if m.isOn then "black" else (colorToCss m.color)
        ] []
      ]
    SetColor c ->
      let
        choiceCircle =
          [ circle
            [ r (toString (buttonSize * 1.50))
            , strokeWidth "2"
            , stroke "black"
            , fill (colorToCss c)
            ] []
          ]
        colorCircle =
          List.map
            (\i ->
              let f = (toFloat i - 0.5) / 12 * 2 * pi
                  x1_ = toPx <| (cos f) * sel.dist
                  y1_ = toPx <| (sin f) * sel.dist
                  t = (toFloat i + 0.5) / 12 * 2 * pi
                  x2_ = toPx <| (cos t) * sel.dist
                  y2_ = toPx <| (sin t) * sel.dist
                  color = Color.hsl f 1 ((sel.dist - buttonSize) / buttonReach)
              in
              Svg.path
                   [ d <| "M " ++ x1_ ++ " " ++ y1_
                       ++ "A " ++ (toPx sel.dist) ++ " " ++ (toPx sel.dist)
                           ++ " 0 0 1 "
                           ++ " " ++ x2_ ++ " " ++ y2_
                   , stroke (colorToCss color)
                   , strokeWidth "5"
                   , fill "none"
                   ] [ ]
            )
            (List.range 0 11)
      in List.concat
        [ choiceCircle
        , colorCircle
        ]

toPx : Float -> String
toPx n = (toString n)

colorToCss : Color -> String
colorToCss color =
  let rgb = toRgb color
  in   "rgb(" ++ toString rgb.red
       ++ "," ++ toString rgb.green
       ++ "," ++ toString rgb.blue
       ++ ")"
