module Layout.Ln exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Layout.Element exposing (..)
import View.Stylesheet as Sty
import Layout.Primitives as Prim


viewInfo : (Ln -> msg) -> Ln -> String -> Element Sty.Style var msg
viewInfo onChange ln key =
    let
        onLnChg =
            (Ln ln.name >> onChange)
    in
        row Sty.None [ spacing 2 ] <|
            [ text ln.name ]
                ++ case ln.lngth of
                    Lng f ->
                        []

                    FltLng f flt ->
                        [ Prim.viewInfoFlt (onLnChg << FltLng f) flt key ]

                    IntLng f int ->
                        [ Prim.viewInfoInt (onLnChg << IntLng f) int key ]


view : Ln -> Length
view ln =
    case ln.lngth of
        Lng f ->
            f

        FltLng f flt ->
            f flt

        IntLng f int ->
            f int
