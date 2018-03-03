module Layout.El exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Layout.Attr as Attr
import Layout.Element exposing (..)
import Layout.Utils as Lutils exposing (Picker(..))
import Utils as U
import View.Stylesheet as Sty


allElemsSorted id =
    allElems id
        |> List.sortWith
            (U.manualOrdering
                [ "el"
                , "row"
                , "column"
                , "wrappedRow"
                , "wrappedColumn"
                , "button"
                , "text"
                , "empty"
                ]
            )


map : (El sty var msg -> El sty var msg) -> Elid -> El sty var msg -> El sty var msg
map fn id node =
    if id == node.id then
        fn node
    else
        case node.elem of
            ElmntElmnt f el ->
                El node.id node.name <| ElmntElmnt f <| map fn id el

            StrElmntElmnt f str el ->
                El node.id node.name <| StrElmntElmnt f str <| map fn id el

            BoolElmntElmnt f bool el ->
                El node.id node.name <| BoolElmntElmnt f bool <| map fn id el

            StyListAttrElmntElmnt f sty attrs el ->
                El node.id node.name <| StyListAttrElmntElmnt f sty attrs <| map fn id el

            FltStyListAttrElmntElmnt f flt sty attrs el ->
                El node.id node.name <| FltStyListAttrElmntElmnt f flt sty attrs <| map fn id el

            ListElmntElmntElmnt f els el ->
                El node.id node.name <| ListElmntElmntElmnt f (List.map (map fn id) els) <| map fn id el

            StyListAttrListElmntElmnt f sty attrs els ->
                El node.id node.name <| StyListAttrListElmntElmnt f sty attrs <| List.map (map fn id) els

            _ ->
                node


mapChildren :
    { childFn : El sty var msg -> El sty var msg
    , childrenFn : List (El sty var msg) -> List (El sty var msg)
    }
    -> El sty var msg
    -> El sty var msg
mapChildren a node =
    case node.elem of
        ElmntElmnt f el ->
            El node.id node.name <|
                ElmntElmnt f <|
                    a.childFn el

        StrElmntElmnt f str el ->
            El node.id node.name <|
                StrElmntElmnt f str <|
                    a.childFn el

        BoolElmntElmnt f bool el ->
            El node.id node.name <|
                BoolElmntElmnt f bool <|
                    a.childFn el

        StyListAttrElmntElmnt f sty attrs el ->
            El node.id node.name <|
                StyListAttrElmntElmnt f sty attrs <|
                    a.childFn el

        FltStyListAttrElmntElmnt f flt sty attrs el ->
            El node.id node.name <|
                FltStyListAttrElmntElmnt f flt sty attrs <|
                    a.childFn el

        ListElmntElmntElmnt f els el ->
            El node.id node.name <|
                ListElmntElmntElmnt f
                    (a.childrenFn els)
                    (a.childFn el)

        StyListAttrListElmntElmnt f sty attrs els ->
            El node.id node.name <|
                StyListAttrListElmntElmnt f sty attrs <|
                    a.childrenFn els

        _ ->
            node


view : (Elid -> List (Attribute var msg)) -> El Sty.Style var msg -> Element Sty.Style var msg
view extraAttrs rootEl =
    let
        viewEls : List (El Sty.Style var msg) -> List (Element Sty.Style var msg)
        viewEls els =
            List.map viewElR els

        viewElR : El Sty.Style var msg -> Element Sty.Style var msg
        viewElR curEl =
            case curEl.elem of
                Elmnt f ->
                    f

                FltElmnt f flt ->
                    f flt

                StrElmnt f str ->
                    f str

                StyElmnt f sty ->
                    f sty

                ElmntElmnt f el_ ->
                    f (viewElR el_)

                StrElmntElmnt f str el_ ->
                    f str (viewElR el_)

                BoolElmntElmnt f bool el_ ->
                    f bool (viewElR el_)

                ListElmntElmntElmnt f els el_ ->
                    f (viewEls els) (viewElR el_)

                StyListAttrStrElmnt f sty attrs str ->
                    f Sty.Elmnt (Attr.viewAll attrs (extraAttrs curEl.id)) str

                StyListAttrElmntElmnt f sty attrs el_ ->
                    f Sty.Elmnt (Attr.viewAll attrs (extraAttrs curEl.id)) (viewElR el_)

                FltStyListAttrElmntElmnt f flt sty attrs el_ ->
                    f flt Sty.Elmnt (Attr.viewAll attrs (extraAttrs curEl.id)) (viewElR el_)

                StyListAttrListElmntElmnt f sty attrs els ->
                    f Sty.Elmnt (Attr.viewAll attrs (extraAttrs curEl.id)) (viewEls els)
    in
    viewElR rootEl


viewTree :
    (Elid -> msg)
    -> Elid
    -> El Sty.Style Sty.Variation msg
    -> Element Sty.Style Sty.Variation msg
viewTree onLabelClick selected node =
    let
        attributes =
            [ paddingLeft 15 ]

        label =
            button (Sty.TreeView Sty.TvLabel)
                [ alignLeft
                , vary Sty.Selected (selected == node.id)
                , onClick <| onLabelClick node.id
                ]
                (text node.name)

        elNoChildren : Element Sty.Style Sty.Variation msg
        elNoChildren =
            el (Sty.TreeView Sty.TvNode) attributes <| label

        elWithChildren : List (El Sty.Style Sty.Variation msg) -> Element Sty.Style Sty.Variation msg
        elWithChildren children =
            column (Sty.TreeView Sty.TvNode) attributes <| label :: List.map (viewTree onLabelClick selected) children
    in
    case node.elem of
        ElmntElmnt f el ->
            elWithChildren [ el ]

        StrElmntElmnt f str el ->
            elWithChildren [ el ]

        BoolElmntElmnt f bool el ->
            elWithChildren [ el ]

        StyListAttrElmntElmnt f sty attrs el ->
            elWithChildren [ el ]

        FltStyListAttrElmntElmnt f flt sty attrs el ->
            elWithChildren [ el ]

        ListElmntElmntElmnt f els el ->
            elWithChildren <| el :: els

        StyListAttrListElmntElmnt f sty attrs els ->
            elWithChildren els

        _ ->
            elNoChildren


viewCode :
    (Elid -> msg)
    -> (String -> msg)
    -> Elid
    -> El Sty.Style Sty.Variation msg
    -> Element Sty.Style Sty.Variation msg
viewCode onLabelClick noneMsg selected node =
    let
        indent =
            4

        viewCodeR :
            List (List String)
            -> Int
            -> El Sty.Style Sty.Variation msg
            -> List (List String)
        viewCodeR acc level node =
            let
                name =
                    node.name

                indnttn =
                    String.repeat (level * indent) " "

                line things =
                    [ [ indnttn, name, " " ] ++ things ]

                visitChild prevLine child =
                    viewCodeR (acc ++ prevLine) (level + 1) child
            in
            case node.elem of
                Elmnt f ->
                    acc ++ line []

                FltElmnt f flt ->
                    acc ++ line [ toString flt ]

                StrElmnt f str ->
                    acc ++ line [ U.quote str ]

                StyElmnt f sty ->
                    acc ++ line [ toString sty ]

                ElmntElmnt f el_ ->
                    visitChild (line [ " <|" ]) el_

                StrElmntElmnt f str el_ ->
                    visitChild (line [ U.quote str, " <|" ]) el_

                BoolElmntElmnt f bool el_ ->
                    visitChild (line [ toString bool, " <|" ]) el_

                StyListAttrStrElmnt f sty attrs str ->
                    acc ++ line [ toString sty, " ", Attr.viewCode attrs, " ", U.quote str ]

                StyListAttrElmntElmnt f sty attrs el_ ->
                    visitChild (line [ toString sty, " ", Attr.viewCode attrs, " <|" ]) el_

                FltStyListAttrElmntElmnt f flt sty attrs el_ ->
                    visitChild (line [ toString flt, " ", toString sty, " ", Attr.viewCode attrs, " <|" ]) el_

                StyListAttrListElmntElmnt f sty attrs els ->
                    let
                        ( firstLine, lastLine ) =
                            if List.isEmpty els then
                                ( line [ toString sty, " ", Attr.viewCode attrs, " []" ], [] )
                            else
                                ( line [ toString sty, " ", Attr.viewCode attrs, " [" ], [ [ indnttn, "]" ] ] )

                        childLines =
                            els
                                |> List.map (viewCodeR [] (level + 1))
                                |> List.indexedMap
                                    (\i lineGroups ->
                                        if i + 1 < List.length els then
                                            List.indexedMap
                                                (\j lineGroup ->
                                                    if j + 1 == List.length lineGroups then
                                                        lineGroup ++ [ "," ]
                                                    else
                                                        lineGroup
                                                )
                                                lineGroups
                                        else
                                            lineGroups
                                    )
                                |> List.concat
                    in
                    acc
                        ++ firstLine
                        ++ childLines
                        ++ lastLine

                ListElmntElmntElmnt f els el_ ->
                    let
                        ( firstLine, lastLine ) =
                            if List.isEmpty els then
                                ( line [ "[]" ], [] )
                            else
                                ( line [ "[" ], [ [ indnttn, "] <|" ] ] )

                        childLines =
                            els
                                |> List.map (viewCodeR [] (level + 1))
                                |> List.indexedMap
                                    (\i lineGroups ->
                                        if i + 1 < List.length els then
                                            List.indexedMap
                                                (\j lineGroup ->
                                                    if j + 1 == List.length lineGroups then
                                                        lineGroup ++ [ "," ]
                                                    else
                                                        lineGroup
                                                )
                                                lineGroups
                                        else
                                            lineGroups
                                    )
                                |> List.concat
                    in
                    acc
                        ++ firstLine
                        ++ childLines
                        ++ visitChild lastLine el_
    in
    Input.multiline (Sty.CodeView Sty.CvTextArea)
        [ height (px 300) ]
        { onChange = noneMsg
        , value = String.join (String.fromChar '\n') <| List.map String.concat <| viewCodeR [] 0 node
        , label = Input.labelAbove <| bold "Code view"
        , options = [ Input.disabled ]
        }


viewInfo :
    { onInsertChild : El Sty.Style var msg -> msg
    , onReplaceEl : El Sty.Style var msg -> msg
    , onSelectEl : Elid -> msg
    , onSelectChild : Elid -> msg
    , onDeleteEl : { bringUpSubtree : Bool } -> Elid -> msg
    , onClickPicker : Picker -> msg
    , noneMsg : msg
    , openPicker : Picker
    , newId : Elid
    , selected : Elid
    , selectedChild : Elid
    , root : El Sty.Style var msg
    }
    -> List (Element Sty.Style Sty.Variation msg)
viewInfo a =
    let
        childEntry child =
            row Sty.None
                [ spacing 5 ]
                [ Lutils.thingButton
                    { style = Sty.NameButton
                    , onThingBttn = a.onSelectEl child.id
                    , showNewThings = False
                    , newThings = []
                    , bttnTxt = "←"
                    , pickerAlignment = alignLeft
                    }
                , button Sty.NameButton
                    [ U.onClickNoProp <| a.onSelectChild child.id
                    , vary Sty.SelectedEntry <| a.selectedChild == child.id
                    ]
                  <|
                    text child.name
                ]

        viewInfoChild : El Sty.Style var msg -> Element Sty.Style Sty.Variation msg
        viewInfoChild child =
            viewInfoChildrenHelper
                { title = text "Child:"
                , children = [ child ]
                , showNewThingButton = False
                }

        viewInfoChildren children =
            viewInfoChildrenHelper
                { title = text "Children:"
                , children = children
                , showNewThingButton = True
                }

        viewInfoChildrenHelper :
            { title : Element Sty.Style Sty.Variation msg
            , children : List (El Sty.Style var msg)
            , showNewThingButton : Bool
            }
            -> Element Sty.Style Sty.Variation msg
        viewInfoChildrenHelper props =
            let
                childEntries =
                    List.map childEntry props.children

                partition before selected after =
                    case after of
                        first :: rest ->
                            if first.id == a.selectedChild then
                                ( before, Just first, rest )
                            else
                                partition (before ++ [ first ]) selected rest

                        [] ->
                            ( before, selected, after )

                ( before, mbSelected, after ) =
                    partition [] Nothing props.children

                selectedWithMenu child =
                    { menu =
                        row Sty.None
                            [ spacing 10 ]
                            [ Lutils.thingButton
                                { style = Sty.Button
                                , onThingBttn = a.onDeleteEl { bringUpSubtree = True } child.id
                                , showNewThings = False
                                , newThings = []
                                , bttnTxt = "d"
                                , pickerAlignment = alignLeft
                                }
                            , Lutils.thingButton
                                { style = Sty.Button
                                , onThingBttn = a.onDeleteEl { bringUpSubtree = False } child.id
                                , showNewThings = False
                                , newThings = []
                                , bttnTxt = "dt"
                                , pickerAlignment = alignLeft
                                }
                            , Lutils.thingButton
                                { style = Sty.Button
                                , onThingBttn = a.onClickPicker <| ReplaceChild child.id
                                , showNewThings = a.openPicker == ReplaceChild child.id
                                , newThings = List.map (Lutils.newThingBttn a.onReplaceEl) <| allElemsSorted child.id
                                , bttnTxt = "r"
                                , pickerAlignment = alignRight
                                }
                            ]
                    , thing = childEntry child
                    }
            in
            Lutils.thingControl
                { title = props.title
                , things =
                    { before = before |> List.map childEntry
                    , selected = mbSelected |> Maybe.map selectedWithMenu
                    , after = after |> List.map childEntry
                    }
                , newThingButton =
                    when props.showNewThingButton <|
                        Lutils.thingButton
                            { style = Sty.Button
                            , onThingBttn = a.onClickPicker AddChild
                            , showNewThings = a.openPicker == AddChild
                            , newThings =
                                List.map (Lutils.newThingBttn a.onInsertChild) <|
                                    allElemsSorted a.newId
                            , bttnTxt = "+"
                            , pickerAlignment = alignLeft
                            }
                }

        replaceThisElement =
            Lutils.thingInfo
                { title = ""
                , newThingBttnTxt = "replace this element..."
                , onNewThingBttn = a.onClickPicker ReplaceElement
                , showNewThings = a.openPicker == ReplaceElement
                , things = []
                , newThings =
                    List.map (Lutils.newThingBttn a.onReplaceEl) <|
                        allElemsSorted a.selected
                }

        mbEl =
            find a.selected a.root

        info : El Sty.Style var msg -> List (Element Sty.Style Sty.Variation msg)
        info thisEl =
            let
                onElemChg : Elem Sty.Style var msg -> msg
                onElemChg =
                    El thisEl.id thisEl.name >> a.onReplaceEl

                key =
                    toString thisEl.id
            in
            case thisEl.elem of
                Elmnt f ->
                    []

                FltElmnt f flt ->
                    [ Lutils.viewInfoFlt (onElemChg << FltElmnt f) flt key ]

                StrElmnt f str ->
                    [ Lutils.viewInfoStr (onElemChg << StrElmnt f) str key ]

                StyElmnt f sty ->
                    []

                ElmntElmnt f el ->
                    [ viewInfoChild el ]

                StrElmntElmnt f str el ->
                    [ Lutils.viewInfoStr (onElemChg << (\str -> StrElmntElmnt f str el)) str key
                    , viewInfoChild el
                    ]

                BoolElmntElmnt f bool el ->
                    [ Lutils.viewInfoBool (onElemChg << (\bool -> BoolElmntElmnt f bool el)) bool key
                    , viewInfoChild el
                    ]

                ListElmntElmntElmnt f els el ->
                    [ viewInfoChild el
                    , viewInfoChildren els
                    ]

                StyListAttrStrElmnt f sty attrs str ->
                    [ Attr.viewInfos (onElemChg << (\attrs -> StyListAttrStrElmnt f sty attrs str)) a.onClickPicker a.openPicker attrs key
                    , Lutils.viewInfoStr (onElemChg << StyListAttrStrElmnt f sty attrs) str key
                    ]

                StyListAttrElmntElmnt f sty attrs el ->
                    [ Attr.viewInfos (onElemChg << (\attrs -> StyListAttrElmntElmnt f sty attrs el)) a.onClickPicker a.openPicker attrs key
                    , viewInfoChild el
                    ]

                FltStyListAttrElmntElmnt f flt sty attrs el ->
                    [ Lutils.viewInfoFlt (onElemChg << (\flt -> FltStyListAttrElmntElmnt f flt sty attrs el)) flt key
                    , Attr.viewInfos (onElemChg << (\attrs -> FltStyListAttrElmntElmnt f flt sty attrs el)) a.onClickPicker a.openPicker attrs key
                    , viewInfoChild el
                    ]

                StyListAttrListElmntElmnt f sty attrs els ->
                    [ Attr.viewInfos (onElemChg << (\attrs -> StyListAttrListElmntElmnt f sty attrs els)) a.onClickPicker a.openPicker attrs key
                    , viewInfoChildren els
                    ]
    in
    case mbEl of
        Just el ->
            [ h1 (Sty.ElementInfo Sty.EiTitle) [ paddingLeft 5, paddingRight 5 ] <| text el.name ]
                ++ info el
                ++ [ replaceThisElement ]

        Nothing ->
            [ h1 (Sty.ElementInfo Sty.EiTitle) [] <| text "Nothing" ]


find : Elid -> El Sty.Style var msg -> Maybe (El Sty.Style var msg)
find id node =
    if node.id == id then
        Just node
    else
        case Debug.log "find" node.elem of
            Elmnt f ->
                Nothing

            FltElmnt f flt ->
                Nothing

            StrElmnt f str ->
                Nothing

            StyElmnt f sty ->
                Nothing

            ElmntElmnt f el ->
                find id el

            StrElmntElmnt f str el ->
                find id el

            BoolElmntElmnt f bool el ->
                find id el

            ListElmntElmntElmnt f els el ->
                finds id <| el :: els

            StyListAttrStrElmnt f sty attrs str ->
                Nothing

            StyListAttrElmntElmnt f sty attrs el ->
                find id el

            FltStyListAttrElmntElmnt f flt sty attrs el ->
                find id el

            StyListAttrListElmntElmnt f sty attrs els ->
                finds id els


finds : Elid -> List (El Sty.Style var msg) -> Maybe (El Sty.Style var msg)
finds id els =
    case els of
        first :: rest ->
            case find id first of
                Just el ->
                    Just el

                Nothing ->
                    finds id rest

        [] ->
            Nothing


deleteOnlyChild : Bool -> Elid -> El Sty.Style var msg -> El Sty.Style var msg
deleteOnlyChild bringUpSubtree id child =
    if child.id /= id then
        child
    else if not bringUpSubtree then
        El child.id "empty" <| Elmnt empty
    else
        case child.elem of
            ElmntElmnt f el ->
                el

            StrElmntElmnt f str el ->
                el

            BoolElmntElmnt f bool el ->
                el

            ListElmntElmntElmnt f els el ->
                el

            StyListAttrElmntElmnt f sty attrs el ->
                el

            FltStyListAttrElmntElmnt f flt sty attrs el ->
                el

            StyListAttrListElmntElmnt f sty attrs els ->
                List.head els |> Maybe.withDefault (El child.id "empty" <| Elmnt empty)

            _ ->
                El child.id "empty" <| Elmnt empty


deleteSibling : Bool -> Elid -> List (El Sty.Style var msg) -> List (El Sty.Style var msg)
deleteSibling bringUpSubtree id children =
    children
        |> List.foldr
            (\child acc ->
                if child.id /= id then
                    child :: acc
                else if not bringUpSubtree then
                    acc
                else
                    case child.elem of
                        ElmntElmnt f el ->
                            el :: acc

                        StrElmntElmnt f str el ->
                            el :: acc

                        BoolElmntElmnt f bool el ->
                            el :: acc

                        ListElmntElmntElmnt f els el ->
                            el :: acc

                        StyListAttrElmntElmnt f sty attrs el ->
                            el :: acc

                        FltStyListAttrElmntElmnt f flt sty attrs el ->
                            el :: acc

                        StyListAttrListElmntElmnt f sty attrs els ->
                            els ++ acc

                        _ ->
                            acc
            )
            []
