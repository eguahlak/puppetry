module Puppetry exposing (..) -- (Message, puppetry, Puppetry, Strip, Pixel, pixelStringOf, graphicsForPuppetry)
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, onMouseDown, onMouseOver)

type alias Pixel =
    { color: String
    , index: Int
    }

type alias Strip =
    { pixelCount: Int
    , name: String
    , pixels: List Pixel
    }

type alias Puppetry =
    { back: Strip
    , middle: Strip
    , front: Strip
    , side: Strip
    , proscenium: Strip
    , color: String
    }

strip: Int -> String -> Strip
strip pixelCount name =
    Strip
        pixelCount
        name
        (List.map (\i -> Pixel "#ff00cc" i) <| List.range 0 (pixelCount - 1))

updatedPixel: Pixel -> Pixel -> Pixel
updatedPixel pixel p =
    if p.index == pixel.index then pixel else p

updatedStrip: Pixel -> Strip  -> Strip
updatedStrip pixel s =
    { s | pixels = List.map (updatedPixel pixel) s.pixels }

puppetry: Puppetry
puppetry =
    Puppetry
        (strip 27 "Bagscene")
        (strip 27 "Mellemscene")
        (strip 27 "Forscene")
        (strip 12 "Sidelys")
        (strip 23 "Proscenie")
        "#ffffff"


-- Update

update: Message -> Puppetry -> Puppetry
update message puppetry =
    case message of
        PixelSelected strip pixel ->
            case strip.name of
                "Bagscene" ->
                    { puppetry | back = updatedStrip { pixel | color = puppetry.color } puppetry.back }
                "Mellemscene" ->
                    { puppetry | middle = updatedStrip { pixel | color = puppetry.color } puppetry.middle }
                "Forscene" ->
                    { puppetry | front = updatedStrip { pixel | color = puppetry.color } puppetry.front }
                "Sidelys" ->
                    { puppetry | side = updatedStrip { pixel | color = puppetry.color } puppetry.side }
                "Proscenie" ->
                    { puppetry | proscenium = updatedStrip { pixel | color = puppetry.color } puppetry.proscenium }
                _ -> puppetry

-- View

type Message
    = PixelSelected Strip Pixel

pixelStringOf: Message -> String
pixelStringOf message =
    case message of
        PixelSelected strip pixel -> (toString pixel.index)++"] in "++strip.name

view: Puppetry -> Html.Html Message
view puppetry =
    svg
        [ width "400"
        , height "200"
        ]
        [ graphicsForStrip puppetry.back       "0"
        , graphicsForStrip puppetry.middle     "30"
        , graphicsForStrip puppetry.front      "60"
        , graphicsForStrip puppetry.proscenium "120"
        ]

graphicsForStrip: Strip -> String -> Svg Message
graphicsForStrip strip pos =
    svg
        [ y pos, width "400", height "20" ]
        (rect
            [ x (toString <| 6*(32 - strip.pixelCount))
            , width (toString (12*strip.pixelCount + 12))
            , height "20"
            ] [ ]
        :: List.map (graphicsForPixel strip) strip.pixels)

graphicsForPixel: Strip -> Pixel -> Svg Message
graphicsForPixel strip pixel =
    let

        x = if pixel.index % 2 == 1 then
                206 + 6*pixel.index
            else 200 - 6*pixel.index

    in
        circle
            [ onMouseOver (PixelSelected strip pixel)
            , cx (toString x)
            , cy "10"
            , r "4"
            , fill pixel.color
            ]
            []

