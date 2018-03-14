module View.View exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html
import Html.Attributes exposing (title)
import Layout.El as El
import Layout.Element as Lyt exposing (El, Elid)
import Model.Model exposing (..)
import Model.Types exposing (Msg(..), Picker(..))
import Utils as U exposing (onClickNoProp)
import View.Stylesheet exposing (..)


view : Model -> Html.Html Msg
view model =
    Element.viewport stylesheet <|
        row Main
            [ width fill, height fill ]
            [ sideBar model
            , renderLayout model
            , when (model.openPicker == SaveLayout) <|
                saveLayoutModal model.saveAsName
            ]


sideBar : Model -> Element Style Variation Msg
sideBar model =
    sidebar None
        [ height fill ]
        [ topMenu
            { title = text <| model.title
            , status = text <| toString model.status
            , canUndo = False
            , canRedo = False
            , openPicker = model.openPicker
            }
        , row None
            [ height fill ]
            [ viewTree model
            , viewElementInfo model
            ]
        , viewCode model
        ]


topMenu :
    { title : Element Style Variation Msg
    , status : Element Style Variation Msg
    , canUndo : Bool
    , canRedo : Bool
    , openPicker : Picker
    }
    -> Element Style Variation Msg
topMenu props =
    let
        menuButton :
            { content : Element Style Variation Msg
            , toolTip : String
            , onClick : Msg
            }
            -> Element Style Variation Msg
        menuButton props =
            el Button
                [ height (px 32)
                , width (px 32)
                , center
                , verticalCenter
                , toAttr <| title props.toolTip
                , onClick props.onClick
                ]
            <|
                el None [ center, verticalCenter ] props.content
    in
    column (TopMenu TmMain)
        [ paddingLeft 5, paddingRight 5, paddingTop 5 ]
        [ row (TopMenu TmTitleBar)
            [ spread ]
            [ el (TopMenu TmTitle) [ verticalCenter ] <|
                props.title
            , el (TopMenu TmStatus) [] <|
                props.status
            ]
        , row (TopMenu TmButtonRow)
            []
            [ menuButton
                { content = text "🗋"
                , toolTip = "New layout"
                , onClick = OnNewLayout
                }
            , menuButton
                { content = text "🗁"
                , toolTip = "Load layout"
                , onClick = OnClickPicker LoadLayout
                }
            , menuButton
                { content = text "🖫"
                , toolTip = "Save layout as..."
                , onClick = OnClickPicker SaveLayout
                }
            , when False <|
                menuButton
                    { content = text "⭳"
                    , toolTip = "Download layout"
                    , onClick = OnClickPicker DownloadLayout
                    }
            , when props.canUndo <|
                menuButton
                    { content = text "↺"
                    , toolTip = "Undo"
                    , onClick = OnUndo
                    }
            , when props.canRedo <|
                menuButton
                    { content = text "↻"
                    , toolTip = "Redo"
                    , onClick = OnRedo
                    }
            ]
        ]


saveLayoutModal name =
    screen <|
        el ModalBackdrop [ height fill, width fill, onClick <| OnClickPicker NonePicker ] <|
            el Modal [ center, verticalCenter, moveUp 100, onClickNoProp NoneMsg ] <|
                column None
                    [ width (px 400) ]
                    [ row None
                        [ spread, padding 10 ]
                        [ bold "Save layout as..."
                        , el Button [ paddingLeft 5, paddingRight 5, onClick <| OnClickPicker NonePicker ] <|
                            text "x"
                        ]
                    , column None
                        [ padding 20, spacing 15 ]
                        [ Input.text
                            InputNormal
                            [ padding 5 ]
                            { onChange = OnSaveAsNameChange
                            , value = name
                            , label = Input.placeholder { text = "Enter a name...", label = Input.hiddenLabel "" }
                            , options = []
                            }
                        , button None [ padding 5, width (px 70), alignRight, onClick OnNewLayout ] <|
                            text "save"
                        ]
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
