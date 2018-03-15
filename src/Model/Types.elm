module Model.Types exposing (..)

import Layout.Element exposing (El, Elid)
import Set exposing (Set)
import View.Stylesheet as Sty exposing (Style, Variation)


type alias State =
    { savedLayouts : Set String
    , lastLayout : String
    }


type alias Layout =
    { layout : El Style Variation Msg
    , newId : Elid
    , title : String
    }


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
    | OnStorageLoadState (Result String State)
    | OnStorageLoadLayout (Result String Layout)
    | OnLoadLayout String
    | OnNewLayout
    | OnSaveAsLayout
    | OnUndo
    | OnRedo
    | OnSaveAsNameChange String
    | SaveState
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
    | NewLayout
    | LoadLayout
    | SaveLayout
    | DownloadLayout
    | NonePicker


type StorageStatus
    = LoadingState
    | LoadingLayout
    | LayoutLoaded
    | SavingState
    | SavingLayout
    | Saved
    | Error String
    | Unavailable
    | LimitExceeded
    | NoneStatus
