module View.View exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html
import Layout.El as El
import Layout.Element as Lyt exposing (El, Elid)
import Model.Model exposing (..)
import View.Stylesheet exposing (..)


button1 : List (Attribute variation msg) -> String -> Element Style variation msg
button1 attrs label =
    button Button (attrs ++ [ padding 5 ]) <| text label


view : Model -> Html.Html Msg
view model =
    Element.viewport stylesheet <|
        row None
            [ width fill, height fill ]
            [ renderLayout model
            , sideBar model
            ]


sideBar : Model -> Element Style Variation Msg
sideBar { selected, newId, openPicker, layout } =
    sidebar ElementInfo [ spacing 20, padding 20, width (px 300) ] <|
        if selected > -1 then
            (El.viewInfo OnInsertChild OnReplaceEl OnClickPicker openPicker newId selected layout)
        else
            [ text "Select an element to begin." ]


renderLayout : Model -> Element Style Variation Msg
renderLayout model =
    let
        mouseOver =
            List.head model.mousedOver |> Maybe.withDefault -1

        selected =
            model.selected

        extraAttrs : Elid -> List (Attribute Variation Msg)
        extraAttrs id =
            [ vary Hover (mouseOver == id)
            , vary Selected (selected == id)
            , onMouseEnter <| OnMouseEnter id
            , onMouseLeave OnMouseLeave
            , onClick <| OnClick id
            ]
    in
        el None [ height fill, width fill ] <|
            El.view extraAttrs model.layout
