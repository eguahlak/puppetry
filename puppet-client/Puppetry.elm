module Puppetry exposing (..)

import List exposing (range)

type alias Color = String

type alias Strip =
    { pixelCount: Int
    , name: String
    , pixels: List Color
    }

type alias Puppetry =
    { back: Strip
    , middle: Strip
    , front: Strip
    , side: Strip
    , proscenium: Strip
    }

strip: Int -> String -> Strip
strip pixelCount name =
    Strip pixelCount name (List.repeat pixelCount "#ff0000")

puppetry: Puppetry
puppetry =
    Puppetry
        (strip 27 "Bagscene")
        (strip 27 "Mellemscene")
        (strip 27 "Forscene")
        (strip 12 "Sidelys")
        (strip 23 "Proscenie")
