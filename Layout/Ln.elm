module Layout.Ln exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Layout.Element exposing (..)
import View.Stylesheet as Sty
import Layout.Primitives as Prim


viewInfo : Ln -> Element Sty.Style var msg
viewInfo ln =
    row Sty.None [ spacing 2 ] <|
        [ text ln.name ]
            ++ case ln.lngth of
                Lng f ->
                    []

                FltLng f flt ->
                    [ Prim.viewInfoFlt flt ]

                IntLng f int ->
                    [ Prim.viewInfoInt int ]


view : Ln -> Length
view ln =
    case ln.lngth of
        Lng f ->
            f

        FltLng f flt ->
            f flt

        IntLng f int ->
            f int
