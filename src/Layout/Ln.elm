module Layout.Ln exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Layout.Element exposing (..)
import Layout.Utils as Lutils
import Model.Types exposing (Picker(..))
import View.Stylesheet as Sty


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
            Ln ln.name >> onChange
    in
    row Sty.None [ spacing 2 ] <|
        [ Lutils.thingButton
            { style = Sty.NameButton
            , onThingBttn = onClickPicker <| ReplaceLength key
            , showNewThings = openPicker == ReplaceLength key
            , newThings = List.map (Lutils.newThingBttn onChange) allLngths
            , bttnTxt = ln.name
            , labelTxt = "replace"
            , pickerAlignment = alignLeft
            }
        ]
            ++ (case ln.lngth of
                    Lng f ->
                        []

                    FltLng f flt ->
                        [ Lutils.viewInfoFlt (onLnChg << FltLng f) flt key ]

                    IntLng f int ->
                        [ Lutils.viewInfoInt (onLnChg << IntLng f) int key ]
               )


view : Ln -> Length
view ln =
    case ln.lngth of
        Lng f ->
            f

        FltLng f flt ->
            f flt

        IntLng f int ->
            f int


viewCode : Ln -> String
viewCode ln =
    case ln.lngth of
        Lng f ->
            ln.name

        FltLng f flt ->
            "(" ++ String.join " " [ ln.name, toString flt ] ++ ")"

        IntLng f int ->
            "(" ++ String.join " " [ ln.name, toString int ] ++ ")"
