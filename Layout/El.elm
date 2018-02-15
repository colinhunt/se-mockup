module Layout.El exposing (..)

import Element exposing (Attribute, Element)
import Layout.Attr exposing (..)
import Layout.Element exposing (..)


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


view : (Elid -> List (Attribute var msg)) -> El sty var msg -> Element sty var msg
view extraAttrs el_ =
    let
        viewEls : List (El sty var msg) -> List (Element sty var msg)
        viewEls els =
            List.map viewElR els

        viewElR : El sty var msg -> Element sty var msg
        viewElR { id, elem } =
            let
                viewAttrsE attrs =
                    viewAttrs attrs ++ extraAttrs id
            in
            case elem of
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
    viewElR el_
