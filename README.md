# Color.HCL

> You should probably use [`kuon/elm-hsluv`](http://package.elm-lang.org/packages/kuon/elm-hsluv/latest). See the note below.

The [HCL colorspace](https://en.wikipedia.org/wiki/HCL_color_space) (also known as CIELUV LCh) is designed such that when you change the **hue**, the perceived lightness doesn't change.

This package deals with that (conversion from/to RGB and the core `Color` type).

Note that **not all argument combinations** (hue, chroma, luminance) **result in a color inside the gamut.** For a colorspace that has similar perceived brightness when changing hues, look at [`HSLuv`](http://www.hsluv.org/).

## Example

![Distinct colors for chroma 1 and luminance 0.5](https://github.com/Janiczek/color-hcl/raw/master/doc/colors.png)

These colors have been generated for chroma 1 and luminance 0.5.
