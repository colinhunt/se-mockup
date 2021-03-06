module View.View exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html
import Layout.El as El
import Layout.Element as Lyt exposing (El, Elid)
import Model.Model exposing (..)
import Model.Types exposing (Msg(..))
import Utils as U
import View.Stylesheet exposing (..)


view : Model -> Html.Html Msg
view model =
    Element.viewport stylesheet <|
        row Main
            [ width fill, height fill ]
            [ sideBar model
            , renderLayout model
            ]


sideBar : Model -> Element Style Variation Msg
sideBar model =
    sidebar None
        [ height fill ]
        [ row None
            [ height fill ]
            [ viewTree model
            , viewElementInfo model
            ]
        , viewCode model
        ]


viewElementInfo { selected, selectedChild, clipped, newId, openPicker, layout } =
    column (ElementInfo EiMain)
        [ padding 1
        , paddingTop 20
        , width (px 300)
        , yScrollbar
        , clipX
        , onClick OnSidebarClick
        ]
    <|
        if selected > -1 then
            El.viewInfo
                { onInsertChild = OnInsertChild
                , onReplaceEl = OnReplaceEl
                , onSelectEl = OnClick
                , onSelectChild = OnSelectChild
                , onDeleteEl = OnDeleteEl
                , onCutEl = OnCutEl
                , onClickPicker = OnClickPicker
                , noneMsg = NoneMsg
                , openPicker = openPicker
                , newId = newId
                , selected = selected
                , selectedChild = selectedChild
                , clipped = clipped
                , root = layout
                }
        else
            [ text "Select an element to begin." ]


viewTree { selected, layout } =
    column None
        [ spacing 1, yScrollbar, padding 5, paddingRight 15 ]
        [ bold "Tree view"
        , El.viewTree OnClick selected layout
        ]


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
    el None [ height fill, width fill, onClick <| OnClick -1 ] <|
        El.view extraAttrs model.layout


viewCode : Model -> Element Style Variation Msg
viewCode { selected, layout } =
    el (CodeView CvMain) [ padding 10, height (px 350) ] <|
        El.viewCode OnClick (always NoneMsg) selected layout
