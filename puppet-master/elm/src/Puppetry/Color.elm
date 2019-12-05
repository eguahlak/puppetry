-- | This is the Color module. All the interesting information
-- about colors are located here.
module Puppetry.Color exposing
  ( Color
  , fromHSL
  , colorToCss
  , decodeColor
  , encodeColor

  , interpolate

  , black
  )

import Color as Color

import Json.Encode as E
import Json.Decode as D

-- | A color is rexported from another library, but let's not think about that
-- too much.
type alias Color = Color.Color

-- | Color from HSL (Hue is an angle in radians, Saturation is [0, 1], and
-- Light is from [0, 1])
fromHSL : (Float, Float, Float) -> Color
fromHSL (h, s, l) = Color.fromHSL (h / pi / 2 * 360, s * 100, l * 100)

colorToCss : Color -> String
colorToCss = Color.toRGBString

decodeColor : D.Decoder Color
decodeColor =
  D.map3 (\r g b -> Color.fromRGB (toFloat r, toFloat g, toFloat b))
    (D.field "red" D.int)
    (D.field "green" D.int)
    (D.field "blue" D.int)

encodeColor : Color -> E.Value
encodeColor c =
    let (r, g, b) = Color.toRGB c
    in E.object
        [ ("red", E.int (round r) )
        , ("green", E.int (round g) )
        , ("blue", E.int (round b) )
        ]

black : Color
black = Color.fromRGB (0, 0, 0)

-- | Given a float in [0,1], and two colors A and B, return a color linearly
-- interpolated between them on the RGB scale
interpolate : Float -> Color -> Color -> Color
interpolate delta from to =
  let
    (sr, sg, sb) = Color.toRGB from
    (er, eg, eb) = Color.toRGB to
    dr = er - sr
    dg = eg - sg
    db = eb - sb
  in
    Color.fromRGB (sr + delta * dr, sg + delta * dg, sb + delta *db)


