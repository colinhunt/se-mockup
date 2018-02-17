module Layout.El exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Layout.Attr exposing (..)
import Layout.Element exposing (..)
import View.Stylesheet as Sty


insert : Elid -> El sty var msg -> El sty var msg -> El sty var msg
insert id newEl node =
    let
        debug0 =
            Debug.log "insertEl" ( newEl.id, node.id )

        match =
            id == node.id
    in
        case node.elem of
            Elmnt f ->
                if match then
                    newEl
                else
                    node

            FltElmnt f flt ->
                if match then
                    newEl
                else
                    node

            StrElmnt f str ->
                if match then
                    newEl
                else
                    node

            StyElmnt f sty ->
                if match then
                    newEl
                else
                    node

            ElmntElmnt f el ->
                El node.id node.name <|
                    ElmntElmnt f <|
                        if match then
                            newEl
                        else
                            insert id newEl el

            StrElmntElmnt f str el ->
                El node.id node.name <|
                    StrElmntElmnt f str <|
                        if match then
                            newEl
                        else
                            insert id newEl el

            BoolElmntElmnt f bool el ->
                El node.id node.name <|
                    BoolElmntElmnt f bool <|
                        if match then
                            newEl
                        else
                            insert id newEl el

            ListElmntElmntElmnt f els el ->
                El node.id node.name <|
                    ListElmntElmntElmnt f
                        (if match then
                            (newEl :: els)
                         else
                            List.map (insert id newEl) els
                        )
                        el

            StyListAttrStrElmnt f sty attrs str ->
                if match then
                    newEl
                else
                    node

            StyListAttrElmntElmnt f sty attrs el ->
                El node.id node.name <|
                    StyListAttrElmntElmnt f sty attrs <|
                        if match then
                            newEl
                        else
                            insert id newEl el

            FltStyListAttrElmntElmnt f flt sty attrs el ->
                El node.id node.name <|
                    FltStyListAttrElmntElmnt f flt sty attrs <|
                        if match then
                            newEl
                        else
                            insert id newEl el

            StyListAttrListElmntElmnt f sty attrs els ->
                El node.id node.name <|
                    StyListAttrListElmntElmnt f sty attrs <|
                        if match then
                            (newEl :: els)
                        else
                            List.map (insert id newEl) els


view : (El sty var msg -> List (Attribute var msg)) -> El sty var msg -> Element sty var msg
view extraAttrs rootEl =
    let
        viewEls : List (El sty var msg) -> List (Element sty var msg)
        viewEls els =
            List.map viewElR els

        viewElR : El sty var msg -> Element sty var msg
        viewElR el_ =
            let
                viewAttrsE attrs =
                    viewAttrs attrs ++ extraAttrs el_
            in
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
                        f sty (viewAttrsE attrs) str

                    StyListAttrElmntElmnt f sty attrs el ->
                        f sty (viewAttrsE attrs) (viewElR el)

                    FltStyListAttrElmntElmnt f flt sty attrs el ->
                        f flt sty (viewAttrsE attrs) (viewElR el)

                    StyListAttrListElmntElmnt f sty attrs els ->
                        f sty (viewAttrsE attrs) (viewEls els)
    in
        viewElR rootEl


viewInfo : (El Sty.Style var msg -> msg) -> Elid -> Elid -> El sty var msg -> List (Element Sty.Style var msg)
viewInfo onAddEl newId id root =
    let
        viewInfoChild : El sty var msg -> Element Sty.Style var msg
        viewInfoChild child =
            column Sty.None
                []
                [ text "Child element:"
                , el Sty.None [ paddingLeft 10 ] <| text child.name
                , el Sty.None [ paddingLeft 10, paddingTop 10 ] <| text "Replace with:"
                , wrappedRow Sty.None [ paddingLeft 20, spacing 5 ] <| List.map newElemButton <| allElems newId
                ]

        viewInfoChildren : List (El sty var msg) -> Element Sty.Style var msg
        viewInfoChildren children =
            column Sty.None [] <|
                text "Children elements:"
                    :: List.map (.name >> text) children
                    ++ [ el Sty.None [ paddingLeft 10, paddingTop 10 ] <| text "Add child:"
                       , wrappedRow Sty.None [ paddingLeft 20, spacing 5 ] <| List.map newElemButton <| allElems newId
                       ]

        newElemButton : El Sty.Style var msg -> Element Sty.Style var msg
        newElemButton newEl =
            el Sty.None [ onClick <| onAddEl newEl ] <| text newEl.name

        mbEl =
            find id root

        info : El sty var msg -> List (Element Sty.Style var msg)
        info el_ =
            case el_.elem of
                Elmnt f ->
                    []

                FltElmnt f flt ->
                    --[ floatInput flt ]
                    []

                StrElmnt f str ->
                    --[ stringInput str ]
                    []

                StyElmnt f sty ->
                    []

                ElmntElmnt f el ->
                    [ viewInfoChild el ]

                StrElmntElmnt f str el ->
                    [ viewInfoChild el ]

                BoolElmntElmnt f bool el ->
                    [ viewInfoChild el ]

                ListElmntElmntElmnt f els el ->
                    [ viewInfoChildren els ]

                StyListAttrStrElmnt f sty attrs str ->
                    []

                StyListAttrElmntElmnt f sty attrs el ->
                    [ viewInfoChild el ]

                FltStyListAttrElmntElmnt f flt sty attrs el ->
                    [ viewInfoChild el ]

                StyListAttrListElmntElmnt f sty attrs els ->
                    [ viewInfoChildren els ]
    in
        case mbEl of
            Just el ->
                (h1 Sty.ElName [] <| text el.name) :: info el

            Nothing ->
                [ h1 Sty.ElName [] <| text "Nothing" ]


find : Elid -> El sty var msg -> Maybe (El sty var msg)
find id node =
    if node.id == id then
        Just node
    else
        case Debug.log "find" node.elem of
            Elmnt f ->
                Nothing

            FltElmnt f flt ->
                --[ floatInput flt ]
                Nothing

            StrElmnt f str ->
                --[ stringInput str ]
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


finds : Elid -> List (El sty var msg) -> Maybe (El sty var msg)
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



--floatInput : Float -> Element sty var msg
--floatInput flt =
--    input OnFloatChange flt "float"
--stringInput : String -> Element sty var msg
--stringInput str =
--    input OnStringChange str "text"
--input : (String -> msg) -> a -> String -> Element sty var msg
--input toMsg value labelTxt =
--    Input.text None
--        { onChange = toMsg
--        , value = value |> toString
--        , label = Input.labelLeft <| text labelTxt
--        , options = []
--        }
