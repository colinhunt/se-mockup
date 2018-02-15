module Layout.Attr exposing (..)

import Element exposing (Attribute)
import Element.Attributes exposing (Length)
import Layout.Element exposing (..)


type alias At var msg =
    { name : String, attr : Attr var msg }


viewAttr : Attr var msg -> Attribute var msg
viewAttr attr =
    case attr of
        Attr f ->
            f

        StrAttr f str ->
            f str

        LngAttr f lng ->
            f (viewLngth lng)

        FltAttr f flt ->
            f flt

        FltFltAttr f flt1 flt2 ->
            f flt1 flt2


viewAttrs : List (Attr var msg) -> List (Attribute var msg)
viewAttrs attrs =
    List.map viewAttr attrs


viewLngth : Lngth -> Length
viewLngth lngth =
    case lngth of
        Lng f ->
            f

        FltLng f flt ->
            f flt

        IntLng f int ->
            f int
