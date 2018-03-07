module Layout.Ln exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Attributes exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Layout.Element exposing (..)
import Layout.Utils as Lutils exposing (Picker(..))
import Utils exposing ((=>))
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


encoder : Ln -> Encode.Value
encoder ln =
    Encode.object
        [ "name" => Encode.string ln.name
        , "lngth" => lngthEncoder ln.name ln.lngth
        ]


lngthEncoder : String -> Lngth -> Encode.Value
lngthEncoder name lngth =
    Encode.object <|
        case lngth of
            Lng f ->
                [ "kind" => Encode.string "Lng"
                , "fn" => Encode.string name
                ]

            FltLng f flt ->
                [ "kind" => Encode.string "FltLng"
                , "fn" => Encode.string name
                , "flt" => Encode.float flt
                ]

            IntLng f int ->
                [ "kind" => Encode.string "IntLng"
                , "fn" => Encode.string name
                , "int" => Encode.int int
                ]


decoder : Decode.Decoder Ln
decoder =
    Decode.map2 Ln
        (Decode.field "name" Decode.string)
        (Decode.field "lngth" decodeLngth)


decodeLngth : Decode.Decoder Lngth
decodeLngth =
    Decode.oneOf
        [ Decode.map Lng
            (Decode.field "fn" <| Lutils.fnDecoder lngFns)
        , Decode.map2 FltLng
            (Decode.field "fn" <| Lutils.fnDecoder fltLngFns)
            (Decode.field "flt" Decode.float)
        , Decode.map2 IntLng
            (Decode.field "fn" <| Lutils.fnDecoder intLngFns)
            (Decode.field "int" Decode.int)
        ]
