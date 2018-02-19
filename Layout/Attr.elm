module Layout.Attr exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Layout.Element exposing (..)
import View.Stylesheet as Sty
import Layout.Ln as Ln
import Layout.Primitives as Prim


view : At var msg -> Attribute var msg
view at =
    case at.attr of
        Attr f ->
            f

        StrAttr f str ->
            f str

        LngAttr f lng ->
            f (Ln.view lng)

        FltAttr f flt ->
            f flt

        FltFltAttr f flt1 flt2 ->
            f flt1 flt2


viewAll : List (At var msg) -> List (Attribute var msg)
viewAll attrs =
    List.map view attrs


viewInfo : At var msg -> Element Sty.Style var msg
viewInfo at =
    row Sty.None [ spacing 5 ] <|
        [ text at.name ]
            ++ case at.attr of
                Attr f ->
                    []

                StrAttr f str ->
                    [ text "str:", text str ]

                LngAttr f lng ->
                    [ Ln.viewInfo lng ]

                FltAttr f flt ->
                    [ Prim.viewInfoFlt flt ]

                FltFltAttr f flt1 flt2 ->
                    [ Prim.viewInfoFlt flt1, Prim.viewInfoFlt flt2 ]


viewInfos : List (At var msg) -> Element Sty.Style var msg
viewInfos attrs =
    column Sty.None [] <| text "Attributes:" :: List.map viewInfo attrs
