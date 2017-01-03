import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Puppetry exposing (Puppetry, puppetry)
import WebSocket

main = program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- Initialization

init: (Model, Cmd message)
init = (Model Puppetry.puppetry "No action yet", Cmd.none)

-- Model

type alias Model =
    { puppetry: Puppetry
    , action: String
    }

-- Update
type Message
    = ColorSelected String
    | Input String
    | Send
    | NewMessage String
    | PuppetryMessage Puppetry.Message

update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        PuppetryMessage subMessage ->
            ( { model
              | action = Puppetry.pixelStringOf subMessage
              , puppetry = Puppetry.update subMessage model.puppetry
              }
            , Cmd.none
            )
        ColorSelected value ->
            let
                p = model.puppetry
            in
                ( { model | puppetry = { p | color = value } }, Cmd.none)
        _ -> (model, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Message
subscriptions model =
    WebSocket.listen "ws://echo.websocket.org" NewMessage


-- View
view : Model -> Html Message
view model =
    div []
        [ div
            [ width 400, style [ ("text-align", "center") ] ]
            [ text "The faboulous Elm Puppetry App" ]
        , Html.map PuppetryMessage (Puppetry.view model.puppetry)
        , div [ style [("border","1px solid red")] ]
            [ text (model.action++"  "++model.puppetry.color)
            , input
                [ type_ "color"
                , onInput ColorSelected
                , value model.puppetry.color ][ ]
            ]
        ]

