module Layout.Primitives exposing (..)

import Element exposing (..)


viewInfoStr str =
    text str


viewInfoFlt flt =
    text <| toString flt


viewInfoInt int =
    text <| toString int


viewInfoBool bool =
    text <| toString bool
