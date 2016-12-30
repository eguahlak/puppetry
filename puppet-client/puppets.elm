import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

main = beginnerProgram
    { model = model
    , update = update
    , view = view
    }

-- Model
type alias Model =
    { back: String
    , middle: String
    , front: String
    , side: String
    , proscenium: String
    }

model = Model "" "" "" "" ""

-- Update
type Message
    = Back String
    | Middle String
    | Front String
    | Side String
    | Proscenium String

update : Message -> Model -> Model
update message model =
    case message of
        Back color -> { model | back = color }
        Middle color -> { model | middle = color }
        Front color -> { model | front = color }
        Side color -> { model | side = color }
        Proscenium color -> { model | proscenium = color }

-- View
view : Model -> Html Message
view model =
    div []
        [ text "Hello World!" ]

