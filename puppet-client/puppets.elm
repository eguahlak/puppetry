import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Puppetry exposing (..)
import WebSocket

main = program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- Initialization

init: (Model, Cmd message)
init = (Model puppetry "", Cmd.none)

-- Model

type alias Model =
    { puppetry: Puppetry
    , action: String
    }

-- Update
type Message
    = PixelClicked Bool
    | Input String
    | Send
    | NewMessage String

update : Message -> Model -> (Model, Cmd Message)
update message model = (model, Cmd.none)
    --case message of
    --    PixelClicked value -> model

-- Subscriptions

subscriptions : Model -> Sub Message
subscriptions model =
    WebSocket.listen "ws://echo.websocket.org" NewMessage


-- View
view : Model -> Html Message
view model =
    div []
        [ div
            []
            [ text "Hello World!" ]
        , viewStrip model.puppetry.back
        , viewStrip model.puppetry.middle
        , viewStrip model.puppetry.front
        , viewStrip model.puppetry.side
        , viewStrip model.puppetry.proscenium
        ]

viewStrip : Strip -> Html Message
viewStrip strip =
    div [ style [("border","1px solid red")] ]
        [ div
            []
            [ text strip.name ]
        , div
            []
            (List.map (viewPixel strip) strip.pixels)
        ]

viewPixel : Strip -> Color -> Html Message
viewPixel strip color =
    input
        [ type_ "checkbox"
        , style
            [ ("background-color", color)
            , ("color", color)
            ]
        , onCheck PixelClicked         ]
        [ ]