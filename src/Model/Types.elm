module Model.Types exposing (..)

import Layout.Element exposing (El, Elid)
import View.Stylesheet as Sty exposing (Style, Variation)


type alias State =
    { layout : El Style Variation Msg, newId : Elid }


type Msg
    = OnInsertChild Int (El Style Variation Msg)
    | OnReplaceEl Elid (El Style Variation Msg)
    | OnDeleteEl { bringUpSubtree : Bool } Elid
    | OnCutEl (El Style Variation Msg)
    | OnMouseEnter Elid
    | OnMouseLeave
    | OnClick Elid
    | OnSelectChild Elid
    | OnClickPicker Picker
    | OnSidebarClick
    | OnLoadState (Result String State)
    | NoneMsg


type Picker
    = ReplaceElement
    | AddChild
    | ReplaceChild Elid
    | DeleteChild Elid
    | AddAttribute
    | ReplaceAttribute String
    | ReplaceLength String
    | InsertAbove
    | InsertBelow Elid
    | None
