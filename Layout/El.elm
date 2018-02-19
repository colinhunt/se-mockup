module Layout.El exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Utils as U
import Layout.Attr as Attr
import Layout.Primitives as Prim
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
                El node.id node.name <| ListElmntElmntElmnt f (List.map (map fn id) els) el

            StyListAttrListElmntElmnt f sty attrs els ->
                El node.id node.name <| StyListAttrListElmntElmnt f sty attrs <| List.map (map fn id) els

            _ ->
                node


insertChild : El sty var msg -> El sty var msg -> El sty var msg
insertChild newEl node =
    case node.elem of
        ElmntElmnt f el ->
            El node.id node.name <|
                ElmntElmnt f <|
                    newEl

        StrElmntElmnt f str el ->
            El node.id node.name <|
                StrElmntElmnt f str <|
                    newEl

        BoolElmntElmnt f bool el ->
            El node.id node.name <|
                BoolElmntElmnt f bool <|
                    newEl

        StyListAttrElmntElmnt f sty attrs el ->
            El node.id node.name <|
                StyListAttrElmntElmnt f sty attrs <|
                    newEl

        FltStyListAttrElmntElmnt f flt sty attrs el ->
            El node.id node.name <|
                FltStyListAttrElmntElmnt f flt sty attrs <|
                    newEl

        ListElmntElmntElmnt f els el ->
            El node.id node.name <|
                ListElmntElmntElmnt f
                    (newEl :: els)
                    el

        StyListAttrListElmntElmnt f sty attrs els ->
            El node.id node.name <|
                StyListAttrListElmntElmnt f sty attrs <|
                    (newEl :: els)

        _ ->
            node


replace : El sty var msg -> El sty var msg -> El sty var msg
replace newEl node =
    newEl


view : (Elid -> List (Attribute var msg)) -> El Sty.Style var msg -> Element Sty.Style var msg
view extraAttrs rootEl =
    let
        viewEls : List (El Sty.Style var msg) -> List (Element Sty.Style var msg)
        viewEls els =
            List.map viewElR els

        viewElR : El Sty.Style var msg -> Element Sty.Style var msg
        viewElR el_ =
            el Sty.Elmnt (extraAttrs el_.id) <|
                case el_.elem of
                    Elmnt f ->
                        f

                    FltElmnt f flt ->
                        f flt

                    StrElmnt f str ->
                        f str

                    StyElmnt f sty ->
                        f sty

                    ElmntElmnt f el ->
                        f (viewElR el)

                    StrElmntElmnt f str el ->
                        f str (viewElR el)

                    BoolElmntElmnt f bool el ->
                        f bool (viewElR el)

                    ListElmntElmntElmnt f els el ->
                        f (viewEls els) (viewElR el)

                    StyListAttrStrElmnt f sty attrs str ->
                        f sty (Attr.viewAll attrs) str

                    StyListAttrElmntElmnt f sty attrs el ->
                        f sty (Attr.viewAll attrs) (viewElR el)

                    FltStyListAttrElmntElmnt f flt sty attrs el ->
                        f flt sty (Attr.viewAll attrs) (viewElR el)

                    StyListAttrListElmntElmnt f sty attrs els ->
                        f sty (Attr.viewAll attrs) (viewEls els)
    in
        viewElR rootEl


viewInfo : (El Sty.Style var msg -> msg) -> (El Sty.Style var msg -> msg) -> Elid -> Elid -> El Sty.Style var msg -> List (Element Sty.Style var msg)
viewInfo onInsertChild onReplaceEl newId selected root =
    let
        listElements : (El Sty.Style var msg -> msg) -> String -> List (Element Sty.Style var msg)
        listElements elMsg label =
            let
                newElemButton : El Sty.Style var msg -> Element Sty.Style var msg
                newElemButton newEl =
                    el Sty.None [ onClick <| elMsg newEl ] <| text newEl.name
            in
                [ el Sty.None [] <| text label
                , wrappedRow Sty.None [ paddingLeft 10, spacing 5 ] <| List.map newElemButton <| allElems newId
                ]

        viewInfoChild : El Sty.Style var msg -> Element Sty.Style var msg
        viewInfoChild child =
            column Sty.None
                []
            <|
                [ text "Child element:"
                , el Sty.None [ paddingLeft 10 ] <| text child.name
                ]
                    ++ listElements onInsertChild "Replace with:"

        viewInfoChildren : List (El Sty.Style var msg) -> Element Sty.Style var msg
        viewInfoChildren children =
            column Sty.None [] <|
                text "Children elements:"
                    :: List.map (.name >> text) children
                    ++ listElements onInsertChild "Add child:"

        replaceThisElement =
            column Sty.None [] <| listElements onReplaceEl "Replace this element with:"

        viewInfoStr : El Sty.Style var msg -> (String -> Elem Sty.Style var msg) -> String -> Element Sty.Style var msg
        viewInfoStr el strElemCtor str =
            Input.text Sty.None
                []
                { onChange = (\txt -> onReplaceEl <| El el.id el.name <| strElemCtor txt)
                , value = str
                , label = Input.labelAbove <| text "Text:"
                , options = []
                }

        mbEl =
            find selected root

        info : El Sty.Style var msg -> List (Element Sty.Style var msg)
        info el_ =
            let
                viewAttrInfos : (List (At var msg) -> Elem Sty.Style var msg) -> List (At var msg) -> Element Sty.Style var msg
                viewAttrInfos attrsElemCtor attrs =
                    Attr.viewInfos el_
                        (\attr ->
                            attrsElemCtor <| List.map (U.when (.name >> (==) attr.name) (always attr)) attrs
                        )
                        onReplaceEl
                        attrs
            in
                case el_.elem of
                    Elmnt f ->
                        []

                    FltElmnt f flt ->
                        [ Prim.viewInfoFlt flt ]

                    StrElmnt f str ->
                        [ viewInfoStr el_ (StrElmnt f) str ]

                    StyElmnt f sty ->
                        []

                    ElmntElmnt f el ->
                        [ viewInfoChild el ]

                    StrElmntElmnt f str el ->
                        [ viewInfoStr el_ (\str -> StrElmntElmnt f str el) str, viewInfoChild el ]

                    BoolElmntElmnt f bool el ->
                        [ Prim.viewInfoBool bool, viewInfoChild el ]

                    ListElmntElmntElmnt f els el ->
                        [ viewInfoChildren els ]

                    StyListAttrStrElmnt f sty attrs str ->
                        [ viewAttrInfos (\attrs -> StyListAttrStrElmnt f sty attrs str) attrs
                        , viewInfoStr el_ (StyListAttrStrElmnt f sty attrs) str
                        ]

                    StyListAttrElmntElmnt f sty attrs el ->
                        [ viewAttrInfos (\attrs -> StyListAttrElmntElmnt f sty attrs el) attrs
                        , viewInfoChild el
                        ]

                    FltStyListAttrElmntElmnt f flt sty attrs el ->
                        [ Prim.viewInfoFlt flt
                        , viewAttrInfos (\attrs -> FltStyListAttrElmntElmnt f flt sty attrs el) attrs
                        , viewInfoChild el
                        ]

                    StyListAttrListElmntElmnt f sty attrs els ->
                        [ viewAttrInfos (\attrs -> StyListAttrListElmntElmnt f sty attrs els) attrs
                        , viewInfoChildren els
                        ]
    in
        case mbEl of
            Just el ->
                [ h1 Sty.ElName [] <| text el.name ] ++ info el ++ [ replaceThisElement ]

            Nothing ->
                [ h1 Sty.ElName [] <| text "Nothing" ]


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
                finds id els

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
