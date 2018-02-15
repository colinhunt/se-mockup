module Model.Model exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import Layout.El exposing (..)
import Layout.Element exposing (..)
import View.Stylesheet exposing (..)


type Msg
    = OnAddEl (El Style Variation Msg)
    | OnMouseEnter Elid
    | OnMouseLeave
    | OnClick Elid


type alias Model =
    { layout : El Style Variation Msg
    , mousedOver : List Elid
    , selected : Elid
    , newId : Elid
    }


initModel : Model
initModel =
    { layout = { id = 0, name = "empty", elem = newEmpty }
    , mousedOver = []
    , selected = -1
    , newId = 42
    }
