module Layout.El exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Utils as U
import Layout.Attr as Attr
import Layout.Utils as Lutils exposing (Picker(..))
import Layout.Element exposing (..)
import View.Stylesheet as Sty


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
    (El sty var msg -> El sty var msg)
    -> (List (El sty var msg) -> List (El sty var msg))
    -> El sty var msg
    -> El sty var msg
mapChildren childFn childrenFn node =
    case node.elem of
        ElmntElmnt f el ->
            El node.id node.name <|
                ElmntElmnt f <|
                    childFn el

        StrElmntElmnt f str el ->
            El node.id node.name <|
                StrElmntElmnt f str <|
                    childFn el

        BoolElmntElmnt f bool el ->
            El node.id node.name <|
                BoolElmntElmnt f bool <|
                    childFn el

        StyListAttrElmntElmnt f sty attrs el ->
            El node.id node.name <|
                StyListAttrElmntElmnt f sty attrs <|
                    childFn el

        FltStyListAttrElmntElmnt f flt sty attrs el ->
            El node.id node.name <|
                FltStyListAttrElmntElmnt f flt sty attrs <|
                    childFn el

        ListElmntElmntElmnt f els el ->
            El node.id node.name <|
                ListElmntElmntElmnt f
                    (childrenFn els)
                    (childFn el)

        StyListAttrListElmntElmnt f sty attrs els ->
            El node.id node.name <|
                StyListAttrListElmntElmnt f sty attrs <|
                    (childrenFn els)

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
            [ height (px 400) ]
            { onChange = noneMsg
            , value = String.join (String.fromChar '\n') <| List.map String.concat <| viewCodeR [] 0 node
            , label = Input.labelAbove <| bold "Code view"
            , options = [ Input.disabled ]
            }


viewInfo :
    (El Sty.Style var msg -> msg)
    -> (El Sty.Style var msg -> msg)
    -> (Elid -> msg)
    -> (Picker -> msg)
    -> msg
    -> Picker
    -> Elid
    -> Elid
    -> El Sty.Style var msg
    -> List (Element Sty.Style var msg)
viewInfo onInsertChild onReplaceEl onSelectEl onClickPicker noneMsg openPicker newId selected root =
    let
        childEntry child =
            row Sty.None
                [ spacing 5 ]
                [ Lutils.thingButton
                    (onClickPicker <| ReplaceChild child.id)
                    (openPicker == ReplaceChild child.id)
                    (List.map (Lutils.newThingBttn onReplaceEl) <| allElems child.id)
                    "r"
                , button Sty.NameButton [ onClick <| onSelectEl child.id ] <|
                    text child.name
                ]

        viewInfoChild : El Sty.Style var msg -> Element Sty.Style var msg
        viewInfoChild child =
            Lutils.thingInfo
                "Child:"
                ""
                noneMsg
                False
                [ childEntry child ]
                []

        viewInfoChildren : List (El Sty.Style var msg) -> Element Sty.Style var msg
        viewInfoChildren children =
            Lutils.thingInfo
                "Children:"
                "+"
                (onClickPicker AddChild)
                (openPicker == AddChild)
                (List.map
                    childEntry
                    children
                )
            <|
                List.map (Lutils.newThingBttn onInsertChild) <|
                    allElems newId

        replaceThisElement =
            Lutils.thingInfo ""
                "replace this element..."
                (onClickPicker ReplaceElement)
                (openPicker == ReplaceElement)
                []
            <|
                List.map (Lutils.newThingBttn onReplaceEl) <|
                    allElems selected

        mbEl =
            find selected root

        info : El Sty.Style var msg -> List (Element Sty.Style var msg)
        info thisEl =
            let
                onElemChg : Elem Sty.Style var msg -> msg
                onElemChg =
                    (El thisEl.id thisEl.name >> onReplaceEl)

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
                        [ viewInfoChild el, viewInfoChildren els ]

                    StyListAttrStrElmnt f sty attrs str ->
                        [ Attr.viewInfos (onElemChg << (\attrs -> StyListAttrStrElmnt f sty attrs str)) onClickPicker openPicker attrs key
                        , Lutils.viewInfoStr (onElemChg << StyListAttrStrElmnt f sty attrs) str key
                        ]

                    StyListAttrElmntElmnt f sty attrs el ->
                        [ Attr.viewInfos (onElemChg << (\attrs -> StyListAttrElmntElmnt f sty attrs el)) onClickPicker openPicker attrs key
                        , viewInfoChild el
                        ]

                    FltStyListAttrElmntElmnt f flt sty attrs el ->
                        [ Lutils.viewInfoFlt (onElemChg << (\flt -> FltStyListAttrElmntElmnt f flt sty attrs el)) flt key
                        , Attr.viewInfos (onElemChg << (\attrs -> FltStyListAttrElmntElmnt f flt sty attrs el)) onClickPicker openPicker attrs key
                        , viewInfoChild el
                        ]

                    StyListAttrListElmntElmnt f sty attrs els ->
                        [ Attr.viewInfos (onElemChg << (\attrs -> StyListAttrListElmntElmnt f sty attrs els)) onClickPicker openPicker attrs key
                        , viewInfoChildren els
                        ]
    in
        case mbEl of
            Just el ->
                [ h1 (Sty.ElementInfo Sty.EiTitle) [] <| text el.name ]
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
    let
        debug =
            Debug.log "finds" id

        first =
            List.head els

        rest =
            List.drop 1 els

        maybeEl =
            case first of
                Just el ->
                    find id el

                Nothing ->
                    first
    in
        case maybeEl of
            Just el ->
                maybeEl

            Nothing ->
                if List.isEmpty rest then
                    Nothing
                else
                    finds id rest
