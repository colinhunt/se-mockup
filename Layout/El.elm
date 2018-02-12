module Layout.El exposing (..)

import Model.Model exposing (..)
import Model.Element exposing (..)
import Model.Attr exposing (..)


type alias Elid =
    Int


type alias El =
    { id : Elid, name : String, el : Elem }


insertEl : El -> El -> El
insertEl el node =
    let
        debug0 =
            Debug.log "insertEl" ( el.id, node.id )
    in
        case node.elem of
            Elmnt f ->
                el

            FltElmnt f flt0 ->
                el

            StrElmnt f str0 ->
                el

            StyElmnt f sty0 ->
                el

            ElmntElmnt f el0 ->
                El node.id <|
                    ElmntElmnt f <|
                        if el.id == node.id then
                            el
                        else
                            (insertEl el el0)

            StrElmntElmnt f str0 el1 ->
                El node.id <|
                    ElmntElmnt f <|
                        if el.id == node.id then
                            el
                        else
                            (insertEl el el1)

            BoolElmntElmnt f bool0 el1 ->
                El node.id <|
                    ElmntElmnt f <|
                        if el.id == node.id then
                            el
                        else
                            (insertEl el el0)

            ListElmntElmntElmnt f els0 el1 ->
                El node.id <|
                    ElmntElmnt f <|
                        if el.id == node.id then
                            el
                        else
                            (insertEl el el0)

            StyListAttrStrElmnt f sty0 attrs1 str2 ->
                el

            StyListAttrElmntElmnt f sty0 attrs1 el2 ->
                el

            FltStyListAttrElmntElmnt f flt0 sty1 attrs2 el3 ->
                el

            StyListAttrListElmntElmnt f sty0 attrs1 els2 ->
                el


viewEl : El -> Element Style Variation Msg
viewEl { id, el } =
    case el.elem of
        Elmnt f ->
            f

        FltElmnt f flt0 ->
            f flt0

        StrElmnt f str0 ->
            f str0

        StyElmnt f sty0 ->
            f sty0

        ElmntElmnt f el0 ->
            f (viewEl el0)

        StrElmntElmnt f str0 el1 ->
            f str0 (viewEl el1)

        BoolElmntElmnt f bool0 el1 ->
            f bool0 (viewEl el1)

        ListElmntElmntElmnt f els0 el1 ->
            f (viewEls els0) (viewEl el1)

        StyListAttrStrElmnt f sty0 attrs1 str2 ->
            f sty0 (viewAttrs attrs1) str2

        StyListAttrElmntElmnt f sty0 attrs1 el2 ->
            f sty0 (viewAttrs attrs1) (viewEl el2)

        FltStyListAttrElmntElmnt f flt0 sty1 attrs2 el3 ->
            f flt0 sty1 (viewAttrs attrs2) (viewEl el3)

        StyListAttrListElmntElmnt f sty0 attrs1 els2 ->
            f sty0 (viewAttrs attrs1) (viewEls els2)


viewEls : List El -> List (Element Style Variation Msg)
viewEls els =
    List.map handleEl els
