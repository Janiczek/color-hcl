module Color.Hcl exposing (HCL, RGB, fromColor, hclToRgb, rgbToHcl, toColor)

{-| Functions for conversion from and to HCL.

Reference for the conversion algorithms: <https://github.com/gka/chroma.js>


# Conversion HCL <-> RGB

@docs hclToRgb, rgbToHcl


# Color

@docs fromColor, toColor


# Types

@docs HCL, RGB

-}

import Basics.Extra
import Color exposing (Color)


{-| HCL record.

Ranges:

  - h: 0..360
  - c: 0..100
  - l: 0..150
  - a: 0..1

See this color picker: <https://bl.ocks.org/mbostock/3e115519a1b495e0bd95>

-}
type alias HCL =
    { hue : Float
    , chroma : Float
    , luminance : Float
    , alpha : Float
    }


type alias LAB =
    { l : Float
    , a : Float
    , b : Float
    , alpha : Float
    }


type alias XYZ =
    { x : Float
    , y : Float
    , z : Float
    , alpha : Float
    }


{-| RGB record.

Ranges:

  - r: 0..255
  - g: 0..255
  - b: 0..255
  - a: 0..1

See this color picker: <https://bl.ocks.org/mbostock/3e115519a1b495e0bd95>

-}
type alias RGB =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


{-| Converts RGB to HCL.
-}
hclToRgb : HCL -> RGB
hclToRgb hcl =
    hcl
        |> hclToLab
        |> labToRgb


hclToLab : HCL -> LAB
hclToLab { hue, chroma, luminance, alpha } =
    let
        hueRadians =
            degrees hue

        a =
            cos hueRadians * chroma

        b =
            sin hueRadians * chroma
    in
    { l = luminance
    , a = a
    , b = b
    , alpha = alpha
    }


labToRgb : LAB -> RGB
labToRgb { l, a, b, alpha } =
    let
        labXyz number =
            if number > 0.206896552 then
                number ^ 3
            else
                0.12841855 * (number - 0.137931034)

        xyzRgb number =
            255
                * (if number <= 0.00304 then
                    12.92 * number
                   else
                    1.055 * number ^ (1 / 2.4) - 0.055
                  )

        xn =
            0.95047

        yn =
            1

        zn =
            1.08883

        x =
            if isNaN a then
                y
            else
                y + a / 500

        y =
            (l + 16) / 116

        z =
            if isNaN b then
                y
            else
                y - b / 200

        xx =
            xn * labXyz x

        yy =
            yn * labXyz y

        zz =
            zn * labXyz z

        red =
            xyzRgb (3.2404542 * xx - 1.5371385 * yy - 0.4985314 * zz)

        green =
            xyzRgb (-0.969266 * xx + 1.8760108 * yy + 0.041556 * zz)

        blue =
            xyzRgb (0.0556434 * xx - 0.2040259 * yy + 1.0572252 * zz)
    in
    { red = red |> round |> clamp 0 255
    , green = green |> round |> clamp 0 255
    , blue = blue |> round |> clamp 0 255
    , alpha = alpha
    }


{-| Converts RGB to HCL.

Reference: <https://en.wikipedia.org/wiki/HCL_color_space#Transformation_from_RGB_to_HCL>

-}
rgbToHcl : RGB -> HCL
rgbToHcl rgb =
    rgb
        |> rgbToLab
        |> labToHcl


rgbToLab : RGB -> LAB
rgbToLab rgb =
    let
        { x, y, z, alpha } =
            rgbToXyz rgb
    in
    { l = 116 * y - 16
    , a = 500 * (x - y)
    , b = 200 * (y - z)
    , alpha = alpha
    }


rgbToXyz : RGB -> XYZ
rgbToXyz { red, green, blue, alpha } =
    let
        rgbXyz number =
            let
                normalized =
                    number / 255
            in
            if normalized <= 0.04045 then
                normalized / 12.92
            else
                ((normalized + 0.055) / 1.055) ^ 2.4

        xyzLab number =
            if number > 0.008856452 then
                number ^ (1 / 3)
            else
                number / 0.12841855 + 0.137931034

        r =
            red |> toFloat |> rgbXyz

        g =
            green |> toFloat |> rgbXyz

        b =
            blue |> toFloat |> rgbXyz

        x =
            xyzLab ((0.4124564 * r + 0.3575761 * g + 0.1804375 * b) / 0.95047)

        y =
            xyzLab ((0.2126729 * r + 0.7151522 * g + 0.072175 * b) / 1)

        z =
            xyzLab ((0.0193339 * r + 0.119192 * g + 0.9503041 * b) / 1.08883)
    in
    { x = x
    , y = y
    , z = z
    , alpha = alpha
    }


floatMod : Float -> Float -> Float
floatMod m number =
    if number > m then
        (number - m)
            |> floatMod m
    else
        number


labToHcl : LAB -> HCL
labToHcl { l, a, b, alpha } =
    let
        nan =
            0 / 0

        hue =
            atan2 b a
                |> Basics.Extra.inDegrees
                |> floatMod 360

        hueFinal =
            if round (chroma * 10000) == 0 then
                nan
            else
                hue

        chroma =
            sqrt (a * a + b * b)

        luminance =
            l
    in
    { hue = hueFinal
    , chroma = chroma
    , luminance = luminance
    , alpha = alpha
    }


{-| Constructs a `Color` from a HCL record.
-}
toColor : HCL -> Color
toColor hcl =
    let
        { red, green, blue } =
            hclToRgb hcl
    in
    Color.rgb red green blue


{-| Constructs a HCL record from a `Color`.
-}
fromColor : Color -> HCL
fromColor color =
    color
        |> Color.toRgb
        |> rgbToHcl
