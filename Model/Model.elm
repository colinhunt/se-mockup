module Model.Model exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import View.Stylesheet exposing (..)
import Layout.Element exposing (..)


type Msg
    = OnAddEl El
    | OnMouseEnter Elid
    | OnMouseLeave
    | OnClick Elid


type alias Elid =
    Int


type alias El =
    { id : Elid, name : String, el : Elem }


type alias Model =
    { layout : El
    , mousedOver : List Elid
    , selected : Elid
    , newId : Elid
    }


initModel =
    { layout = { id = 0, name = "empty", el = newEmpty }
    , mousedOver = []
    , selected = -1
    , newId = 42
    }
