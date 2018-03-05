module Model.Model exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import Layout.El exposing (..)
import Layout.Element exposing (..)
import Layout.Utils exposing (Picker(..))
import View.Stylesheet as Sty exposing (Style, Variation)


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
    | NoneMsg


type alias Model =
    { layout : El Style Variation Msg
    , mousedOver : List Elid
    , selected : Elid
    , selectedChild : Elid
    , clipped : Maybe (El Style Variation Msg)
    , newId : Elid
    , openPicker : Picker
    }


initModel : Model
initModel =
    { layout =
        { id = 0
        , name = "el"
        , elem =
            StyListAttrElmntElmnt el
                Sty.None
                [ { name = "padding", attr = FltAttr padding 20 }
                , { name = "center", attr = Attr center }
                , { name = "verticalCenter", attr = Attr verticalCenter }
                ]
                { id = 1, name = "text", elem = StrElmnt text "Click to edit!" }
        }
    , mousedOver = []
    , selected = 0
    , selectedChild = -1
    , clipped = Nothing
    , newId = 10
    , openPicker = None
    }
