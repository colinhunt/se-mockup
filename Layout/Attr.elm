module Layout.Attr exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Utils as U
import Layout.Element exposing (..)
import View.Stylesheet as Sty
import Layout.Ln as Ln
import Layout.Utils as Lutils exposing (Picker(..))


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


viewAll : List (At var msg) -> List (Attribute var msg) -> List (Attribute var msg)
viewAll attrs extraAttrs =
    extraAttrs ++ List.map view attrs


viewInfo :
    (At var msg -> msg)
    -> (Picker -> msg)
    -> Picker
    -> String
    -> At var msg
    -> Element Sty.Style var msg
viewInfo onChange onClickPicker openPicker key at =
    let
        onAttrChg : Attr var msg -> msg
        onAttrChg =
            (At at.name >> onChange)
    in
        row Sty.None [ spacing 5 ] <|
            [ Lutils.thingButton
                (onClickPicker <| ReplaceAttribute at.name)
                (openPicker == ReplaceAttribute at.name)
                (List.map (Lutils.newThingBttn onChange) allAttrs)
                at.name
            ]
                ++ case at.attr of
                    Attr f ->
                        []

                    StrAttr f str ->
                        [ Lutils.viewInfoStr (onAttrChg << StrAttr f) str key ]

                    LngAttr f lng ->
                        [ Ln.viewInfo (onAttrChg << LngAttr f) onClickPicker openPicker lng key ]

                    FltAttr f flt ->
                        [ Lutils.viewInfoFlt (onAttrChg << FltAttr f) flt key ]

                    FltFltAttr f flt1 flt2 ->
                        [ Lutils.viewInfoFlt (onAttrChg << (\flt -> FltFltAttr f flt flt2)) flt1 key
                        , Lutils.viewInfoFlt (onAttrChg << (\flt -> FltFltAttr f flt1 flt)) flt2 key
                        ]


viewInfos :
    (List (At var msg) -> msg)
    -> (Picker -> msg)
    -> Picker
    -> List (At var msg)
    -> String
    -> Element Sty.Style var msg
viewInfos onChange onClickPicker openPicker attrs key =
    let
        onAttrsChg oldAttr newAttr =
            onChange <| List.map (U.when (.name >> (==) oldAttr.name) (always newAttr)) attrs
    in
        Lutils.thingInfo
            "Attributes:"
            "add..."
            (onClickPicker AddAttribute)
            (openPicker == AddAttribute)
            (List.map (\at -> viewInfo (onAttrsChg at) onClickPicker openPicker (key ++ at.name) at) attrs)
        <|
            List.map
                (Lutils.newThingBttn <|
                    \attr ->
                        onChange <| attr :: attrs
                )
                allAttrs


viewCode : List (At var msg) -> String
viewCode attrs =
    let
        viewCode_ : At var msg -> String
        viewCode_ at =
            case at.attr of
                Attr f ->
                    at.name

                StrAttr f str ->
                    String.join " " [ at.name, U.quote str ]

                LngAttr f lng ->
                    String.join " " [ at.name, Ln.viewCode lng ]

                FltAttr f flt ->
                    String.join " " [ at.name, toString flt ]

                FltFltAttr f flt1 flt2 ->
                    String.join " " [ at.name, toString flt1, toString flt2 ]
    in
        "[" ++ (String.join ", " <| List.map viewCode_ attrs) ++ "]"
