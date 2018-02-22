module Model.Model exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import Layout.El exposing (..)
import Layout.Element exposing (..)
import Layout.Utils exposing (Picker(..))
import View.Stylesheet as Sty exposing (Style, Variation)


type Msg
    = OnInsertChild (El Style Variation Msg)
    | OnReplaceEl (El Style Variation Msg)
    | OnMouseEnter Elid
    | OnMouseLeave
    | OnClick Elid
    | OnClickPicker Picker
    | OnSidebarClick


type alias Model =
    { layout : El Style Variation Msg
    , mousedOver : List Elid
    , selected : Elid
    , newId : Elid
    , openPicker : Picker
    }


initModel : Model
initModel =
    { layout =
        { id = 0
        , name = "text"
        , elem = StrElmnt text "Click to edit"
        }
    , mousedOver = []
    , selected = -1
    , newId = 2
    , openPicker = None
    }
