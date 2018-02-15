module View.View exposing (..)

import Html
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import View.Stylesheet exposing (..)
import Model.Model exposing (..)
import Layout.El exposing (viewEl)


button1 : List (Attribute variation msg) -> String -> Element Style variation msg
button1 attrs label =
    button Button (attrs ++ [ padding 5 ]) <| text label


view : Model -> Html.Html Msg
view model =
    Element.viewport stylesheet <|
        row None
            [ width fill, height fill ]
            [ renderLayout model
            , sideBar model.selected
            ]


sideBar : El -> Element Style Variation Msg
sideBar el =
    column None
        [ spacing 20 ]
        (elementInfo el)


elementInfo : El -> List (Element Style Variation Msg)
elementInfo el =
    [ button1 [ onClick OnAddEl ] "add el"
    ]


renderLayout : Model -> Element Style Variation Msg
renderLayout model =
    let
        mouseOver =
            List.head model.mousedOver |> Maybe.withDefault -1

        selected =
            model.selected

        handleAttr : Attr -> Attribute Variation Msg
        handleAttr attr =
            case attr of
                A f ->
                    f

                LA f lngth ->
                    f (handleLngth lngth)

                FA f float ->
                    f float

                FFA f float1 float2 ->
                    f float1 float2

        handleLngth : Lngth -> Length
        handleLngth lngth =
            case lngth of
                L f ->
                    f

                FL f float ->
                    f float

                IL f int ->
                    f int

        handleEls : List El -> List (Element Style Variation Msg)
        handleEls els =
            List.map handleEl els

        handleAttrs : Elid -> List Attr -> List (Attribute Variation Msg)
        handleAttrs id attrs =
            [ vary Hover (mouseOver == id)
            , vary Selected (selected == id)
            , onMouseEnter <| OnMouseEnter id
            , onMouseLeave OnMouseLeave
            , onClick <| OnClick id
            ]
                ++ (List.map handleAttr attrs)
    in
        handleEl model.layout
