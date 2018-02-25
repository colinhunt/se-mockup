module View.View exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html
import Utils as U
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
            [ viewTree model
            , sideBar model
            , renderLayout model
                |> within [ viewCode model ]
            ]


sideBar : Model -> Element Style Variation Msg
sideBar { selected, newId, openPicker, layout } =
    sidebar ElementInfo [ spacing 20, padding 20, width (px 300), onClick OnSidebarClick ] <|
        if selected > -1 then
            (El.viewInfo OnInsertChild OnReplaceChild OnReplaceChildren OnReplaceEl OnClickPicker openPicker newId selected layout)
        else
            [ text "Select an element to begin." ]


viewTree { selected, layout } =
    el None [ spacing 1, scrollbars, paddingRight 15 ] <|
        El.viewTree OnClick selected layout


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
            , U.onClickNoProp <| OnClick id
            ]
    in
        el None [ height fill, width fill ] <|
            El.view extraAttrs model.layout


viewCode : Model -> Element Style Variation Msg
viewCode { selected, layout } =
    el CodeView [ alignBottom, width fill, height (px 400), scrollbars ] <|
        El.viewCode OnClick (always NoneMsg) selected layout
