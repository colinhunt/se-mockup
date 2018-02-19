module Layout.Attr exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Utils as U
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


viewInfos :
    El Sty.Style var msg
    -> (At var msg -> Elem Sty.Style var msg)
    -> (El Sty.Style var msg -> msg)
    -> List (At var msg)
    -> Element Sty.Style var msg
viewInfos el attrElmntCtor onReplaceEl attrs =
    let
        viewInfo : At var msg -> Element Sty.Style var msg
        viewInfo at =
            let
                viewInfoFlt : (Float -> Attr var msg) -> Float -> Element Sty.Style var msg
                viewInfoFlt fltAttrCtor flt =
                    Input.text Sty.None
                        []
                        { onChange =
                            (\txt ->
                                onReplaceEl <|
                                    El el.id el.name <|
                                        attrElmntCtor <|
                                            At at.name <|
                                                fltAttrCtor <|
                                                    Result.withDefault flt <|
                                                        U.strToPosFlt txt
                            )
                        , value = toString flt
                        , label = Input.hiddenLabel "Float"
                        , options = []
                        }
            in
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
                                [ viewInfoFlt (FltAttr f) flt ]

                            FltFltAttr f flt1 flt2 ->
                                [ viewInfoFlt (\flt -> FltFltAttr f flt flt2) flt1
                                , viewInfoFlt (\flt -> FltFltAttr f flt1 flt) flt2
                                ]
    in
        column Sty.None [] <| text "Attributes:" :: List.map viewInfo attrs
