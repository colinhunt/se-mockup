module View.View exposing (..)

import Html
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import View.Stylesheet exposing (..)
import Model.Model exposing (..)
import Update.Update as Update


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
    [ button1 [ onClick (Fun Update.onAddEl) ] "add el"
    ]


renderLayout : Model -> Element Style Variation Msg
renderLayout model =
    let
        mouseOver =
            List.head model.mousedOver |> Maybe.withDefault -1

        selected =
            model.selected

        handleEl : El -> Element Style Variation Msg
        handleEl { id, el } =
            case el of
                E f ->
                    f

                StrE f str ->
                    f str

                StrEE f str el ->
                    f str (handleEl el)

                SE f style ->
                    f style

                SAEE f style attrs el ->
                    f style (handleAttrs id attrs) (handleEl el)

                SALE f style attrs els ->
                    f style (handleAttrs id attrs) (handleEls els)

                SAStrE f style attrs str ->
                    f style (handleAttrs id attrs) str

                EE f el ->
                    f (handleEl el)

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
            , onMouseEnter <| Fun <| Update.onMouseEnter id
            , onMouseLeave <| Fun Update.onMouseLeave
            , onClick <| Fun <| Update.onClick id
            ]
                ++ (List.map handleAttr attrs)
    in
        handleEl model.layout
