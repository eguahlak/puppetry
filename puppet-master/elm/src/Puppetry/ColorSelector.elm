module Puppetry.ColorSelector exposing (..)

import Color exposing (Color, fromRGB, toRGB, toHSL)
import String exposing (fromFloat, fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Puppetry.Utilities exposing (..)

import Html.Events.Extra.Touch as Touch

-- MODEL

type alias ColorSelector =
  { color : Color
  , active : Bool
  , state : State
  }

init : Color -> Bool -> ColorSelector
init c a =
  { color = c
  , active = a
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
    Setting { dist, angle } -> "Setting dist:"++(fromFloat dist)++" angle:"++(fromFloat angle)
    Switching { dist, angle } -> "Setting dist:"++(fromFloat dist)++" angle:"++(fromFloat angle)

-- VIEW

buttonSize : Float
buttonSize = 40

buttonReach : Float
buttonReach = 200

type alias Config msg =
  { x : Int
  , y : Int
  , onChange : ColorSelector -> msg
  , onSelection : ColorSelector -> msg
  }

view : Config msg -> ColorSelector -> Svg msg
view config model =
  g [ translate config
    , Touch.onStart (handleOnChange config model)
    , Touch.onMove (handleOnChange config model)
    , Touch.onEnd (handleOnEnd config model)
    ] <|
    List.concat
      [ [ circle
          [ r "150"
          , fillOpacity "0"
          ] [] ]
      , viewSelection model
      , viewButton config model
      ]

viewButton : Config msg -> ColorSelector -> List (Svg msg)
viewButton config model =
    [ circle
      [ r (fromFloat buttonSize)
      , strokeWidth "2"
      , stroke "black"
      , fill (if (model.active) then (colorToCss model.color) else "black")
      ] []
    , circle
      [ r (fromFloat (buttonSize / 3))
      , strokeWidth "0"
      , stroke "black"
      , fill (colorToCss model.color)
      ] []
    ]

viewSelection : ColorSelector -> List (Svg msg)
viewSelection { color, active, state } =
  case state of
    Passive -> []
    Switching _ ->
      [ circle
        [ r (fromFloat (buttonSize * 1.50))
        , strokeWidth "2"
        , stroke "black"
        , fill <| if active then "black" else (colorToCss color)
        ] []
      ]
    Setting selection ->
      let
        choiceCircle =
          [ circle
            [ r (fromFloat (buttonSize * 1.50))
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
                  c = Color.fromHSL (f, 1, ((selection.dist - buttonSize) / buttonReach))
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
toPx n = (fromFloat n)



-- UTILS

translate : Config msg -> Attribute msg
translate c = transform ("translate("++fromInt c.x++", "++fromInt c.y++")")

positionFromCoordinates : Config msg -> Touch.Event -> Position
positionFromCoordinates config event =
  let
    (x, y) = touchCoordinates event
  in
    { x = x - (toFloat config.x)
    , y = y - (toFloat config.y) 
    }

touchCoordinates : Touch.Event -> (Float, Float)
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )
        

handleOnChange : Config msg -> ColorSelector -> Touch.Event -> msg
handleOnChange config model event =
 let
   p = positionFromCoordinates config event
   s = selectionFromPosition p
 in
   config.onChange (modelChangeFromSelection s model)

handleOnEnd : Config msg -> ColorSelector -> Touch.Event -> msg
handleOnEnd config model event =
 let
   p = positionFromCoordinates config event
   s = selectionFromPosition p
 in
   config.onSelection (modelEndFromSelection s model)

stateOfSelection : Selection -> State
stateOfSelection selection =
  if selection.dist < buttonSize then Switching selection
  else if selection.dist < (buttonSize + buttonReach) then Setting selection
  else Passive

modelChangeFromSelection : Selection -> ColorSelector -> ColorSelector
modelChangeFromSelection selection model =
  case stateOfSelection selection of
    Passive -> { model | state = Passive }
    Setting sel ->
      { model
      | state = Setting sel
      -- , color = colorFromSelection sel
      }
    Switching sel -> { model | state = Switching sel }

modelEndFromSelection : Selection -> ColorSelector -> ColorSelector
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
  Color.fromHSL (angle, 1, ((dist - buttonSize)/buttonReach))
