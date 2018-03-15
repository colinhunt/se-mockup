module View.View exposing (..)

import Color exposing (Color)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html
import Html.Attributes exposing (title)
import Layout.El as El
import Layout.Element as Lyt exposing (El, Elid)
import Material.Icons.Content as Icons
import Material.Icons.File as Icons
import Material.Icons.Navigation as Icons
import Model.Model exposing (..)
import Model.Types exposing (Msg(..), Picker(..))
import Set
import Svg exposing (Svg, svg)
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
                myModal <|
                    dialogue
                        { title = "Save layout as..."
                        , content =
                            saveAsContent model.saveAsName
                        }
            , when (model.openPicker == LoadLayout) <|
                myModal <|
                    dialogue
                        { title = "Load layout..."
                        , content =
                            loadLayoutPicker <|
                                Set.toList model.state.savedLayouts
                        }
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


iconButton :
    { icon : Color -> Int -> Svg Msg
    , toolTip : String
    , onClick : Msg
    , size : Int
    , padding : Int
    }
    -> Element Style Variation Msg
iconButton props =
    el Button
        [ toAttr <| title props.toolTip
        , onClick props.onClick
        , height (px <| toFloat <| props.size + props.padding)
        , width (px <| toFloat <| props.size + props.padding)
        ]
    <|
        el None
            [ center
            , verticalCenter
            , height (px <| toFloat props.size)
            , width (px <| toFloat props.size)
            ]
        <|
            html <|
                svg [ Html.Attributes.height props.size, Html.Attributes.width props.size ] <|
                    [ props.icon iconColor props.size ]


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
        menuButton props =
            iconButton
                { icon = props.icon
                , toolTip = props.toolTip
                , onClick = props.onClick
                , size = 24
                , padding = 10
                }
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
            [ verticalCenter ]
            [ menuButton
                { icon = Icons.add
                , toolTip = "New layout"
                , onClick = OnNewLayout
                }
            , menuButton
                { icon = Icons.folder_open
                , toolTip = "Load layout"
                , onClick = OnClickPicker LoadLayout
                }
            , menuButton
                { icon = Icons.save
                , toolTip = "Save layout as..."
                , onClick = OnClickPicker SaveLayout
                }
            , when True <|
                menuButton
                    { icon = Icons.file_download
                    , toolTip = "Download layout"
                    , onClick = OnClickPicker DownloadLayout
                    }
            , when True <|
                menuButton
                    { icon = Icons.undo
                    , toolTip = "Undo"
                    , onClick = OnUndo
                    }
            , when True <|
                menuButton
                    { icon = Icons.redo
                    , toolTip = "Redo"
                    , onClick = OnRedo
                    }
            ]
        ]


myModal content =
    screen <|
        el ModalBackdrop [ height fill, width fill, onClick <| OnClickPicker NonePicker ] <|
            el Modal [ center, verticalCenter, moveUp 100, onClickNoProp NoneMsg ] <|
                content


dialogue : { title : String, content : Element Style Variation Msg } -> Element Style Variation Msg
dialogue props =
    column None
        [ width (px 400), padding 10, spacing 20 ]
        [ row None
            [ spread ]
            [ bold props.title
            , iconButton
                { icon = Icons.close
                , toolTip = "Close"
                , onClick = OnClickPicker NonePicker
                , size = 20
                , padding = 0
                }
            ]
        , props.content
        ]


saveAsContent name =
    column None
        [ padding 10, spacing 15 ]
        [ Input.text
            InputNormal
            [ padding 5 ]
            { onChange = OnSaveAsNameChange
            , value = name
            , label = Input.placeholder { text = "Enter a name...", label = Input.hiddenLabel "" }
            , options = []
            }
        , button None [ padding 5, width (px 70), alignRight, onClick OnSaveAsLayout ] <|
            text "save"
        ]


loadLayoutPicker names =
    column None
        []
    <|
        List.map layoutEntry names


layoutEntry name =
    el Button [ onClick <| OnLoadLayout name ] <| text name


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
