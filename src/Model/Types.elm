module Model.Types exposing (..)

import Layout.Element exposing (Elid)


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
