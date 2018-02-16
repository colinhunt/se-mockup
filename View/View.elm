module View.View exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html
import Layout.El as El
import Layout.Element exposing (El, newEmpty)
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
            , sideBar model.selected
            ]


sideBar : Maybe (El Style Variation Msg) -> Element Style Variation Msg
sideBar el =
    sidebar ElementInfo [ spacing 20, padding 20, width (px 300) ] <|
        case el of
            Just el ->
                (El.viewInfo el)

            Nothing ->
                [ text "Select an element to begin." ]


renderLayout : Model -> Element Style Variation Msg
renderLayout model =
    let
        mouseOver =
            List.head model.mousedOver |> Maybe.withDefault -1

        selected =
            model.selected |> Maybe.withDefault { id = -1, name = "null", elem = newEmpty }

        extraAttrs : El Style Variation Msg -> List (Attribute Variation Msg)
        extraAttrs el =
            [ vary Hover (mouseOver == el.id)
            , vary Selected (selected.id == el.id)
            , onMouseEnter <| OnMouseEnter el.id
            , onMouseLeave OnMouseLeave
            , onClick <| OnClick el
            ]
    in
        El.view extraAttrs model.layout
