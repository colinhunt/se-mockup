module Layout.El exposing (..)

import Element exposing (..)
import Element.Input as Input
import Layout.Attr exposing (..)
import Layout.Element exposing (..)
import View.Stylesheet exposing (Style)


insert : El sty var msg -> El sty var msg -> El sty var msg
insert el_ node =
    let
        debug0 =
            Debug.log "insertEl" ( el_.id, node.id )
    in
        case node.elem of
            Elmnt f ->
                el_

            FltElmnt f flt ->
                el_

            StrElmnt f str ->
                el_

            StyElmnt f sty ->
                el_

            ElmntElmnt f el ->
                El node.id node.name <| ElmntElmnt f el_

            StrElmntElmnt f str el ->
                El node.id node.name <| StrElmntElmnt f str el_

            BoolElmntElmnt f bool el ->
                El node.id node.name <| BoolElmntElmnt f bool el_

            ListElmntElmntElmnt f els el ->
                El node.id node.name <| ListElmntElmntElmnt f (el_ :: els) el

            StyListAttrStrElmnt f sty attrs str ->
                el_

            StyListAttrElmntElmnt f sty attrs el ->
                El node.id node.name <| StyListAttrElmntElmnt f sty attrs el_

            FltStyListAttrElmntElmnt f flt sty attrs el ->
                El node.id node.name <| FltStyListAttrElmntElmnt f flt sty attrs el_

            StyListAttrListElmntElmnt f sty attrs els ->
                El node.id node.name <| StyListAttrListElmntElmnt f sty attrs (el_ :: els)


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


viewInfo : El sty var msg -> List (Element sty var msg)
viewInfo el_ =
    let
        info : List (Element sty var msg)
        info =
            case el_.elem of
                Elmnt f ->
                    []

                FltElmnt f flt ->
                    [ floatInput flt ]

                StrElmnt f str ->
                    [ stringInput str ]

                StyElmnt f sty ->
                    []

                ElmntElmnt f el ->
                    []

                StrElmntElmnt f str el ->
                    []

                BoolElmntElmnt f bool el ->
                    []

                ListElmntElmntElmnt f els el ->
                    []

                StyListAttrStrElmnt f sty attrs str ->
                    []

                StyListAttrElmntElmnt f sty attrs el ->
                    []

                FltStyListAttrElmntElmnt f flt sty attrs el ->
                    []

                StyListAttrListElmntElmnt f sty attrs els ->
                    []
    in
        (h1 ElName [] <| text el_.name) :: info


floatInput : Float -> Element sty var msg
floatInput flt =
    input OnFloatChange flt "float"


stringInput : String -> Element sty var msg
stringInput str =
    input OnStringChange str "text"


input : (String -> msg) -> a -> String -> Element sty var msg
input toMsg value labelTxt =
    Input.text None
        { onChange = toMsg
        , value = value |> toString
        , label = Input.labelLeft <| text labelTxt
        , options = []
        }
