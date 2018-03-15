module Model.Model exposing (..)

import Data.Storage
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import Json.Decode exposing (Value)
import Layout.El exposing (..)
import Layout.Element exposing (..)
import Model.Types exposing (Msg, Picker(..), State, StorageStatus(..))
import Set exposing (Set)
import View.Stylesheet as Sty exposing (Style, Variation)


productName =
    "Haywire"


type alias Model =
    { layout : El Style Variation Msg
    , state : State
    , mousedOver : List Elid
    , selected : Elid
    , selectedChild : Elid
    , clipped : Maybe (El Style Variation Msg)
    , newId : Elid
    , openPicker : Picker
    , title : String
    , status : StorageStatus
    , saveAsName : String
    }


init : ( Model, Cmd Msg )
init =
    { layout =
        { id = 0
        , name = "el"
        , elem =
            StyListAttrElmntElmnt el
                Sty.None
                [ { name = "padding"
                  , attr = FltAttr padding 20
                  }
                , { name = "center"
                  , attr = Attr center
                  }
                , { name = "verticalCenter"
                  , attr = Attr verticalCenter
                  }
                ]
                { id = 1
                , name = "text"
                , elem = StrElmnt text "Click to edit!"
                }
        }
    , state = { savedLayouts = Set.empty, lastLayout = "" }
    , mousedOver = []
    , selected = 0
    , selectedChild = -1
    , clipped = Nothing
    , newId = 10
    , openPicker = NonePicker
    , title = "Welcome to " ++ productName
    , status = LoadingState
    , saveAsName = ""
    }
        ! [ Data.Storage.loadState ]
