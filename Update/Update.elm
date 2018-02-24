module Update.Update exposing (..)

import Element exposing (empty)
import Utils as U
import Layout.El as El
import Layout.Element exposing (El, Elid, Elem(Elmnt))
import Layout.Utils exposing (Picker(..))
import Model.Model exposing (..)
import View.Stylesheet exposing (Style, Variation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInsertChild el ->
            onInsertChild el model

        OnReplaceChild el ->
            onReplaceChild el model

        OnReplaceChildren el ->
            onReplaceChildren el model

        OnReplaceEl el ->
            onReplaceEl (Debug.log "OnReplaceEl" el) model

        OnMouseEnter id ->
            onMouseEnter id model

        OnMouseLeave ->
            onMouseLeave model

        OnClick id ->
            onClick id model

        OnClickPicker picker ->
            onClickPicker picker model

        OnSidebarClick ->
            { model | openPicker = None } ! []


onInsertChild : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onInsertChild elem m =
    insertReplace (El.map <| El.mapChildren identity <| (::) elem) m


onReplaceChild : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onReplaceChild elem m =
    insertReplace
        (El.map <|
            El.mapChildren
                (always elem)
                identity
        )
        m


onReplaceChildren : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onReplaceChildren elem m =
    insertReplace
        (El.map <|
            El.mapChildren
                identity
                (List.map (U.when (.id >> (==) elem.id) (always elem)))
        )
        m


onReplaceEl : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onReplaceEl elem m =
    insertReplace (El.map <| El.replace elem) m


insertReplace : (Elid -> El Style Variation Msg -> El Style Variation Msg) -> Model -> ( Model, Cmd Msg )
insertReplace f m =
    { m
        | layout = f m.selected m.layout
        , newId = m.newId + 10
    }
        ! []


onMouseEnter id model =
    { model | mousedOver = id :: model.mousedOver } ! []


onMouseLeave model =
    { model | mousedOver = model.mousedOver |> List.drop 1 } ! []


onClick : Elid -> Model -> ( Model, Cmd Msg )
onClick id model =
    let
        mouseOver =
            List.head model.mousedOver |> Maybe.withDefault -1

        selected =
            model.selected

        model_ =
            { model | openPicker = None }
    in
        if id >= 0 then
            if id == selected then
                { model_ | selected = -1 } ! []
            else
                { model_ | selected = id } ! []
        else
            model_ ! []


onClickPicker : Picker -> Model -> ( Model, Cmd Msg )
onClickPicker picker model =
    let
        picker_ =
            if model.openPicker == picker then
                None
            else
                picker
    in
        { model | openPicker = picker_ } ! []
