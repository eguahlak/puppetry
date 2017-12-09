module Puppetry.ColorSelector exposing (..)

import Color exposing (Color, rgb, toRgb, toHsl)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Puppetry.Utilities exposing (..)

import Touch
import SingleTouch

-- MODEL

type alias ColorSelection =
  { color : Color
  , active : Bool
  , state : State
  }

init : Color -> ColorSelection
init c =
  { color = c
  , active = False
  , state = Passive
  }

-- UPDATE

type State
  = Passive
  | Setting Selection
  | Switching Selection

toText : State -> String
toText state =
  case state of
    Passive -> "Passive"
    Setting { dist, angle } -> "Setting dist:"++(toString dist)++" angle:"++(toString angle)
    Switching { dist, angle } -> "Switching dist:"++(toString dist)++" angle:"++(toString angle)

-- VIEW

buttonSize : Float
buttonSize = 40

buttonReach : Float
buttonReach = 200

type alias Config msg =
  { x : Int
  , y : Int
  , onChange : ColorSelection -> msg
  }

view : Config msg -> ColorSelection -> Svg msg
view config model =
  g [ translate config
    , SingleTouch.onStart (handleOnChange config model)
    , SingleTouch.onMove (handleOnChange config model)
    , SingleTouch.onEnd (handleOnEnd config model)
    ] <|
    List.concat
      [ [ circle
          [ r "150"
          , fillOpacity "0"
          ] [] ]
      , viewSelection model
      , viewButton config model
      ]

viewButton : Config msg -> ColorSelection -> List (Svg msg)
viewButton config model =
    [ circle
      [ r (toString buttonSize)
      , strokeWidth "2"
      , stroke "black"
      , fill (if (model.active) then (colorToCss model.color) else "black")
      ] []
    , circle
      [ r (toString (buttonSize / 3))
      , strokeWidth "0"
      , stroke "black"
      , fill (colorToCss model.color)
      ] []
    ]

viewSelection : ColorSelection -> List (Svg msg)
viewSelection { color, active, state } =
  case state of
    Passive -> []
    Switching _ ->
      [ circle
        [ r (toString (buttonSize * 1.50))
        , strokeWidth "2"
        , stroke "black"
        , fill <| if active then "black" else (colorToCss color)
        ] []
      ]
    Setting selection ->
      let
        choiceCircle =
          [ circle
            [ r (toString (buttonSize * 1.50))
            , strokeWidth "2"
            , stroke "black"
            , fill (colorToCss (colorFromSelection selection))
            ] []
          ]
        colorCircle =
          List.map
            (\i ->
              let f = (toFloat i - 0.5) / 12 * 2 * pi
                  x1_ = toPx <| (cos f) * selection.dist
                  y1_ = toPx <| (sin f) * selection.dist
                  t = (toFloat i + 0.5) / 12 * 2 * pi
                  x2_ = toPx <| (cos t) * selection.dist
                  y2_ = toPx <| (sin t) * selection.dist
                  c = Color.hsl f 1 ((selection.dist - buttonSize) / buttonReach)
              in
                Svg.path
                   [ d <| "M " ++ x1_ ++ " " ++ y1_
                       ++ "A " ++ (toPx selection.dist) ++ " " ++ (toPx selection.dist)
                           ++ " 0 0 1 "
                           ++ " " ++ x2_ ++ " " ++ y2_
                   , stroke (colorToCss c)
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



-- UTILS

translate : Config msg -> Attribute msg
translate c = transform ("translate("++toString c.x++", "++toString c.y++")")

positionFromCoordinates : Config msg -> Touch.Coordinates -> Position
positionFromCoordinates config coordinates =
  let
    (x, y) = Touch.clientPos coordinates
  in
    { x = x - (toFloat config.x), y = y - (toFloat config.y) }


handleOnChange : Config msg -> ColorSelection -> Touch.Coordinates -> msg
handleOnChange config model coordinates =
 let
   p = positionFromCoordinates config coordinates
   s = selectionFromPosition p
 in
   config.onChange (modelChangeFromSelection s model)

handleOnEnd : Config msg -> ColorSelection -> Touch.Coordinates -> msg
handleOnEnd config model coordinates =
 let
   p = positionFromCoordinates config coordinates
   s = selectionFromPosition p
 in
   config.onChange (modelEndFromSelection s model)

stateOfSelection : Selection -> State
stateOfSelection selection =
  if selection.dist < buttonSize then Switching selection
  else if selection.dist < (buttonSize + buttonReach) then Setting selection
  else Passive

modelChangeFromSelection : Selection -> ColorSelection -> ColorSelection
modelChangeFromSelection selection model =
  case stateOfSelection selection of
    Passive -> { model | state = Passive }
    Setting sel ->
      { model
      | state = Setting sel
      -- , color = colorFromSelection sel
      }
    Switching sel -> { model | state = Switching sel }

modelEndFromSelection : Selection -> ColorSelection -> ColorSelection
modelEndFromSelection selection model =
  case stateOfSelection selection of
    Passive -> { model | state = Passive }
    Setting sel ->
      { model
      | state = Passive
      , color = colorFromSelection sel
      }
    Switching _ ->
      { model
      | state = Passive
      , active = not model.active
      }

colorFromSelection : Selection -> Color
colorFromSelection { dist, angle } =
  Color.hsl angle 1 ((dist - buttonSize)/buttonReach)
