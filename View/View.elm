module View.View exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html
import Layout.El as El
import Layout.Element exposing (El)
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

            --, sideBar model.selected
            ]


sideBar : El Style Variation Msg -> Element Style Variation Msg
sideBar el =
    column None
        [ spacing 20 ]
        (elementInfo el)


elementInfo : El Style Variation Msg -> List (Element Style Variation Msg)
elementInfo el =
    [ text el.name
    ]


renderLayout : Model -> Element Style Variation Msg
renderLayout model =
    let
        mouseOver =
            List.head model.mousedOver |> Maybe.withDefault -1

        selected =
            model.selected

        extraAttrs id =
            [ vary Hover (mouseOver == id)
            , vary Selected (selected == id)
            , onMouseEnter <| OnMouseEnter id
            , onMouseLeave OnMouseLeave
            , onClick <| OnClick id
            ]
    in
    El.view extraAttrs model.layout
