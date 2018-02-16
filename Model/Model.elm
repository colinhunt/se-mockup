module Model.Model exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import Layout.El exposing (..)
import Layout.Element exposing (..)
import View.Stylesheet as Sty exposing (Style, Variation)


type Msg
    = OnAddEl (El Style Variation Msg)
    | OnMouseEnter Elid
    | OnMouseLeave
    | OnClick (El Style Variation Msg)


type alias Model =
    { layout : El Style Variation Msg
    , mousedOver : List Elid
    , selected : Maybe (El Style Variation Msg)
    , newId : Elid
    }


initModel : Model
initModel =
    { layout =
        { id = 0
        , name = "el"
        , elem =
            newEl Sty.Elmnt
                [ newWidth newFill, newHeight newFill ]
                { id = -1
                , name = "text"
                , elem = newText "Click to edit"
                }
        }
    , mousedOver = []
    , selected = Nothing
    , newId = 1
    }
