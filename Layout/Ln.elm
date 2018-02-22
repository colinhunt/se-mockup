module Layout.Ln exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Layout.Element exposing (..)
import View.Stylesheet as Sty
import Layout.Utils as Lutils exposing (Picker(..))


viewInfo :
    (Ln -> msg)
    -> (Picker -> msg)
    -> Picker
    -> Ln
    -> String
    -> Element Sty.Style var msg
viewInfo onChange onClickPicker openPicker ln key =
    let
        onLnChg =
            (Ln ln.name >> onChange)
    in
        row Sty.None [ spacing 2 ] <|
            [ Lutils.thingButton
                (onClickPicker <| ReplaceLength key)
                (openPicker == ReplaceLength key)
                (List.map (Lutils.newThingBttn onChange) allLngths)
                ln.name
            ]
                ++ case ln.lngth of
                    Lng f ->
                        []

                    FltLng f flt ->
                        [ Lutils.viewInfoFlt (onLnChg << FltLng f) flt key ]

                    IntLng f int ->
                        [ Lutils.viewInfoInt (onLnChg << IntLng f) int key ]


view : Ln -> Length
view ln =
    case ln.lngth of
        Lng f ->
            f

        FltLng f flt ->
            f flt

        IntLng f int ->
            f int
