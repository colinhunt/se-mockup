module Update.Update exposing (..)

import Element exposing (empty)
import Layout.El as El
import Layout.Element exposing (El, Elem(Elmnt), Elid)
import Layout.Utils exposing (Picker(..))
import Model.Model exposing (..)
import Utils as U
import View.Stylesheet exposing (Style, Variation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInsertChild el ->
            onInsertChild el model

        OnReplaceEl el ->
            onReplaceEl (Debug.log "OnReplaceEl" el) model

        OnDeleteEl { bringUpSubtree } id ->
            onDeleteEl bringUpSubtree id model

        OnMouseEnter id ->
            onMouseEnter id model

        OnMouseLeave ->
            onMouseLeave model

        OnClick id ->
            onClick id model

        OnSelectChild id ->
            onSelectChild id model

        OnClickPicker picker ->
            onClickPicker picker model

        OnSidebarClick ->
            { model | openPicker = None, selectedChild = -1 } ! []

        NoneMsg ->
            model ! []


onInsertChild : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onInsertChild elem m =
    insertReplace
        (El.map
            (El.mapChildren
                { childFn = identity
                , childrenFn = \l -> l ++ [ elem ]
                }
            )
            m.selected
            m.layout
        )
        m


onReplaceEl : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onReplaceEl elem m =
    insertReplace (El.map (always elem) elem.id m.layout) m


onDeleteEl : Bool -> Elid -> Model -> ( Model, Cmd Msg )
onDeleteEl bringUpSubtree id model =
    let
        newLayout =
            El.map
                (El.mapChildren
                    { childFn = El.deleteOnlyChild bringUpSubtree id
                    , childrenFn = El.deleteSibling bringUpSubtree id
                    }
                )
                model.selected
                model.layout
    in
    { model | layout = newLayout } ! []


insertReplace : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
insertReplace newLayout m =
    { m
        | layout = newLayout
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


onSelectChild : Elid -> Model -> ( Model, Cmd Msg )
onSelectChild id model =
    if id == model.selectedChild then
        { model | selectedChild = -1 } ! []
    else
        { model | selectedChild = id } ! []


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
