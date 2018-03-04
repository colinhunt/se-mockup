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


allElemsSorted id children =
    allElems id children
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
            button Sty.NameButton
                [ alignLeft
                , onClick <| onLabelClick node.id
                , vary Sty.SelectedEntry <| selected == node.id
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
    { onInsertChild : Int -> El Sty.Style var msg -> msg
    , onReplaceEl : Elid -> El Sty.Style var msg -> msg
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
viewInfo props =
    let
        childEntry child =
            row Sty.None
                [ spacing 5 ]
                [ button Sty.NameButton
                    [ U.onClickNoProp <| props.onSelectChild child.id
                    , vary Sty.SelectedEntry <| props.selectedChild == child.id
                    ]
                  <|
                    text child.name
                , Lutils.thingButton
                    { style = Sty.Button
                    , onThingBttn = props.onSelectEl child.id
                    , showNewThings = False
                    , newThings = []
                    , bttnTxt = "➟"
                    , labelTxt = "go to"
                    , pickerAlignment = alignLeft
                    }
                ]

        viewInfoChild : El Sty.Style var msg -> Element Sty.Style Sty.Variation msg
        viewInfoChild child =
            viewInfoChildrenHelper
                { title = text "Child:"
                , children = [ child ]
                , showNewThingButton = False
                , onChildrenChange = always props.noneMsg
                }

        viewInfoChildren onChildrenChange children =
            viewInfoChildrenHelper
                { title = text "Children:"
                , children = children
                , showNewThingButton = True
                , onChildrenChange = onChildrenChange
                }

        viewInfoChildrenHelper :
            { title : Element Sty.Style Sty.Variation msg
            , children : List (El Sty.Style var msg)
            , showNewThingButton : Bool
            , onChildrenChange : List (El Sty.Style var msg) -> msg
            }
            -> Element Sty.Style Sty.Variation msg
        viewInfoChildrenHelper props2 =
            let
                childEntries =
                    List.map childEntry props2.children

                partition before selected after =
                    case after of
                        first :: rest ->
                            if first.id == props.selectedChild then
                                ( before, Just first, rest )
                            else
                                partition (before ++ [ first ]) selected rest

                        [] ->
                            ( before, selected, after )

                ( before, mbSelected, after ) =
                    partition [] Nothing props2.children

                reorderChild direction id children =
                    let
                        reorder acc remaining =
                            case remaining of
                                first :: second :: rest ->
                                    if first.id == id then
                                        acc ++ [ second, first ] ++ rest
                                    else
                                        reorder (acc ++ [ first ]) (second :: rest)

                                first :: [] ->
                                    acc ++ remaining

                                [] ->
                                    acc
                    in
                    case direction of
                        Lutils.Up ->
                            children |> List.reverse |> reorder [] |> List.reverse

                        Lutils.Down ->
                            children |> reorder []

                selectedWithMenu index child =
                    { menu =
                        row Sty.None
                            [ spacing 10 ]
                            [ when (List.length props2.children > 1) <|
                                Lutils.thingButton
                                    { style = Sty.Button
                                    , onThingBttn = props2.onChildrenChange <| reorderChild Lutils.Up child.id props2.children
                                    , showNewThings = False
                                    , newThings = []
                                    , bttnTxt = "↑"
                                    , labelTxt = "move up"
                                    , pickerAlignment = alignLeft
                                    }
                            , when (List.length props2.children > 1) <|
                                Lutils.thingButton
                                    { style = Sty.Button
                                    , onThingBttn = props2.onChildrenChange <| reorderChild Lutils.Down child.id props2.children
                                    , showNewThings = False
                                    , newThings = []
                                    , bttnTxt = "↓"
                                    , labelTxt = "move down"
                                    , pickerAlignment = alignLeft
                                    }
                            , Lutils.thingButton
                                { style = Sty.Button
                                , onThingBttn = props.onClickPicker <| InsertBelow child.id
                                , showNewThings = props.openPicker == InsertBelow child.id
                                , newThings =
                                    List.map (Lutils.newThingBttn <| props.onReplaceEl child.id) <|
                                        allElemsSorted props.newId [ child ]
                                , bttnTxt = "^"
                                , labelTxt = "insert"
                                , pickerAlignment = alignRight
                                }
                            , Lutils.thingButton
                                { style = Sty.Button
                                , onThingBttn = props.onClickPicker <| ReplaceChild child.id
                                , showNewThings = props.openPicker == ReplaceChild child.id
                                , newThings =
                                    List.map (Lutils.newThingBttn <| props.onReplaceEl child.id) <|
                                        allElemsSorted child.id <|
                                            getChildren child
                                , bttnTxt = "r"
                                , labelTxt = "replace"
                                , pickerAlignment = alignRight
                                }
                            , Lutils.thingButton
                                { style = Sty.Button
                                , onThingBttn = props.onInsertChild (index + 1) <| El props.newId child.name <| child.elem
                                , showNewThings = False
                                , newThings = []
                                , bttnTxt = "©"
                                , labelTxt = "copy"
                                , pickerAlignment = alignLeft
                                }
                            , Lutils.thingButton
                                { style = Sty.Button
                                , onThingBttn = props.onDeleteEl { bringUpSubtree = True } child.id
                                , showNewThings = False
                                , newThings = []
                                , bttnTxt = "d"
                                , labelTxt = "delete"
                                , pickerAlignment = alignLeft
                                }
                            , Lutils.thingButton
                                { style = Sty.Button
                                , onThingBttn = props.onDeleteEl { bringUpSubtree = False } child.id
                                , showNewThings = False
                                , newThings = []
                                , bttnTxt = "dt"
                                , labelTxt = "delete tree"
                                , pickerAlignment = alignLeft
                                }
                            ]
                    , thing = childEntry child
                    }
            in
            Lutils.thingControl
                { title = props2.title
                , things =
                    { before = before |> List.map childEntry
                    , selected = mbSelected |> Maybe.map (selectedWithMenu <| List.length before)
                    , after = after |> List.map childEntry
                    }
                , newThingButton =
                    when props2.showNewThingButton <|
                        Lutils.thingButton
                            { style = Sty.Button
                            , onThingBttn = props.onClickPicker AddChild
                            , showNewThings = props.openPicker == AddChild
                            , newThings =
                                List.map (Lutils.newThingBttn <| props.onInsertChild -1) <|
                                    allElemsSorted props.newId []
                            , bttnTxt = "+"
                            , labelTxt = "add new"
                            , pickerAlignment = alignLeft
                            }
                }

        replaceThisElement children =
            Lutils.thingInfo
                { title = ""
                , newThingBttnTxt = "replace this element..."
                , onNewThingBttn = props.onClickPicker ReplaceElement
                , showNewThings = props.openPicker == ReplaceElement
                , things = []
                , newThings =
                    List.map (Lutils.newThingBttn <| props.onReplaceEl props.selected) <|
                        allElemsSorted props.selected children
                }

        insertAboveThis this =
            Lutils.thingInfo
                { title = ""
                , newThingBttnTxt = "insert element above..."
                , onNewThingBttn = props.onClickPicker InsertAbove
                , showNewThings = props.openPicker == InsertAbove
                , things = []
                , newThings =
                    List.map (Lutils.newThingBttn <| props.onReplaceEl props.selected) <|
                        allElemsSorted props.newId [ this ]
                }

        mbThis =
            find props.selected props.root

        info : El Sty.Style var msg -> List (Element Sty.Style Sty.Variation msg)
        info thisEl =
            let
                onElemChg : Elem Sty.Style var msg -> msg
                onElemChg =
                    El thisEl.id thisEl.name >> props.onReplaceEl thisEl.id

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

                StyListAttrStrElmnt f sty attrs str ->
                    [ Attr.viewInfos
                        (onElemChg << (\attrs -> StyListAttrStrElmnt f sty attrs str))
                        props.onClickPicker
                        props.openPicker
                        attrs
                        key
                    , Lutils.viewInfoStr (onElemChg << StyListAttrStrElmnt f sty attrs) str key
                    ]

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

                StyListAttrElmntElmnt f sty attrs el ->
                    [ Attr.viewInfos
                        (onElemChg << (\attrs -> StyListAttrElmntElmnt f sty attrs el))
                        props.onClickPicker
                        props.openPicker
                        attrs
                        key
                    , viewInfoChild el
                    ]

                FltStyListAttrElmntElmnt f flt sty attrs el ->
                    [ Lutils.viewInfoFlt
                        (onElemChg << (\flt -> FltStyListAttrElmntElmnt f flt sty attrs el))
                        flt
                        key
                    , Attr.viewInfos
                        (onElemChg << (\attrs -> FltStyListAttrElmntElmnt f flt sty attrs el))
                        props.onClickPicker
                        props.openPicker
                        attrs
                        key
                    , viewInfoChild el
                    ]

                StyListAttrListElmntElmnt f sty attrs els ->
                    [ Attr.viewInfos
                        (onElemChg << (\attrs -> StyListAttrListElmntElmnt f sty attrs els))
                        props.onClickPicker
                        props.openPicker
                        attrs
                        key
                    , viewInfoChildren (onElemChg << StyListAttrListElmntElmnt f sty attrs) els
                    ]

                ListElmntElmntElmnt f els el ->
                    [ viewInfoChild el
                    , viewInfoChildren (onElemChg << (\els -> ListElmntElmntElmnt f els el)) els
                    ]
    in
    case mbThis of
        Just this ->
            [ h1 (Sty.ElementInfo Sty.EiTitle) [ paddingLeft 5, paddingRight 5 ] <| text this.name ]
                ++ info this
                ++ [ replaceThisElement <| getChildren this
                   , insertAboveThis this
                   ]

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
        getChildren child |> List.head |> Maybe.withDefault (El child.id "empty" <| Elmnt empty)


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
                    getChildren child ++ acc
            )
            []


getChildren : El Sty.Style var msg -> List (El Sty.Style var msg)
getChildren elem =
    case elem.elem of
        ElmntElmnt f el ->
            [ el ]

        StrElmntElmnt f str el ->
            [ el ]

        BoolElmntElmnt f bool el ->
            [ el ]

        ListElmntElmntElmnt f els el ->
            [ el ]

        StyListAttrElmntElmnt f sty attrs el ->
            [ el ]

        FltStyListAttrElmntElmnt f flt sty attrs el ->
            [ el ]

        StyListAttrListElmntElmnt f sty attrs els ->
            els

        _ ->
            []
