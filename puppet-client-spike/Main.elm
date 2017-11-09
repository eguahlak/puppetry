import Touch
import SingleTouch
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlA

main = Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }

-- model

type alias Model = { x : Float, y : Float }

model : Model
model = Model 50.0 40.0

-- UPDATE

type Message
  = TouchStart Touch.Coordinates
  | TouchMove Touch.Coordinates
  | TouchEnd Touch.Coordinates
  | TouchCancel Touch.Coordinates

coordinateModel : Touch.Coordinates -> Model
coordinateModel coordinates =
  let
    (x, y) = Touch.clientPos coordinates
  in
    Model x y

update : Message -> Model -> Model
update message model =
  case message of
    TouchStart coordinates ->
      coordinateModel coordinates

    TouchMove coordinates ->
      coordinateModel coordinates

    TouchEnd coordinates ->
      coordinateModel coordinates

    TouchCancel coordinates ->
      coordinateModel coordinates

-- VIEW

view : Model -> Html Message
view model =
  svg
    (HtmlA.style
      [ ("border", "1px solid red")
      , ("width", "500px")
      , ("height", "500px")
      ] :: touchEvents )
    [
      g []
        [ line
          [ strokeWidth "5px "
          , stroke "#ff0000"
          , x1 "0"
          , y1 "0"
          , x2 (toString model.x)
          , y2 (toString model.y)
          ]
        []
      ]
    ]

touchEvents : List (Html.Attribute Message)
touchEvents =
    [ SingleTouch.onStart TouchStart
    , SingleTouch.onMove TouchMove
    , SingleTouch.onEnd TouchEnd
    , SingleTouch.onCancel TouchCancel
    ]
