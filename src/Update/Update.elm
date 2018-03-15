module Update.Update exposing (..)

import Data.Storage
import Element exposing (empty)
import Json.Decode exposing (Value)
import Layout.El as El
import Layout.Element exposing (El, Elem(Elmnt), Elid)
import Model.Model exposing (Model)
import Model.Types exposing (Layout, Msg(..), Picker(..), State, StorageStatus(..))
import Set exposing (Set)
import Utils as U
import View.Stylesheet exposing (Style, Variation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        OnInsertChild index el ->
            onInsertChild index el model

        OnReplaceEl id el ->
            onReplaceEl (Debug.log "OnReplaceEl" el) id model

        OnDeleteEl { bringUpSubtree } id ->
            onDeleteEl bringUpSubtree id model

        OnCutEl el ->
            onCutEl el model

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
            { model | openPicker = NonePicker, selectedChild = -1 } ! []

        OnLoadState result ->
            onLoadState result model

        OnLoadLayout result ->
            onLoadLayout result model

        OnSaveAsLayout ->
            onSaveAsLayout model

        OnNewLayout ->
            onNewLayout model

        OnUndo ->
            onUndo model

        OnRedo ->
            onRedo model

        OnSaveAsNameChange name ->
            onSaveAsNameChange name model

        SaveState ->
            saveState model

        NoneMsg ->
            model ! []


onInsertChild : Int -> El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onInsertChild index elem model =
    let
        childrenFn children =
            let
                index_ =
                    if index < 0 then
                        List.length children
                    else
                        index
            in
            List.take index_ children ++ [ elem ] ++ List.drop index_ children
    in
    insertReplace model <|
        El.map
            (El.mapChildren
                { childFn = identity
                , childrenFn = childrenFn
                }
            )
            model.selected
            model.layout


onReplaceEl : El Style Variation Msg -> Elid -> Model -> ( Model, Cmd Msg )
onReplaceEl elem id model =
    insertReplace model <| El.map (always elem) id model.layout


onDeleteEl : Bool -> Elid -> Model -> ( Model, Cmd Msg )
onDeleteEl bringUpSubtree id model =
    insertReplace model <| deleteChild bringUpSubtree id model.selected model.layout


onCutEl : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onCutEl elem model =
    let
        ( newModel, cmd ) =
            insertReplace model <| deleteChild False elem.id model.selected model.layout
    in
    { newModel | clipped = Just elem } ! [ cmd ]


insertReplace : Model -> El Style Variation Msg -> ( Model, Cmd Msg )
insertReplace ({ state } as model) newLayout =
    let
        newModel =
            { model
                | layout = newLayout
                , newId = model.newId + 10
                , status = SavingLayout
            }
    in
    newModel
        ! [ Data.Storage.saveLayout
                { layout = newModel.layout
                , newId = newModel.newId
                , title = model.title
                }
          ]


onMouseEnter id model =
    { model | mousedOver = id :: model.mousedOver } ! []


onMouseLeave model =
    { model | mousedOver = model.mousedOver |> List.drop 1 } ! []


onClick : Elid -> Model -> ( Model, Cmd Msg )
onClick id ({ mousedOver, selected } as model) =
    let
        mouseOver =
            List.head mousedOver |> Maybe.withDefault -1
    in
    if id == selected || id < 0 then
        { model | selected = -1, openPicker = NonePicker } ! []
    else
        { model | selected = id, openPicker = NonePicker } ! []


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
                NonePicker
            else
                picker
    in
    { model | openPicker = picker_ } ! []


deleteChild bringUpSubtree id selected layout =
    El.map
        (El.mapChildren
            { childFn = El.deleteOnlyChild bringUpSubtree id
            , childrenFn = El.deleteSibling bringUpSubtree id
            }
        )
        selected
        layout


onLoadState : Result String State -> Model -> ( Model, Cmd Msg )
onLoadState result ({ state } as model) =
    case result of
        Result.Ok loadedState ->
            if not <| String.isEmpty loadedState.lastLayout then
                { model | state = loadedState, status = LoadingLayout }
                    ! [ Data.Storage.loadLayout loadedState.lastLayout ]
            else
                let
                    newState =
                        { state | lastLayout = model.title }
                in
                { model | state = newState, status = SavingState }
                    ! [ Data.Storage.saveState newState
                      , Data.Storage.saveLayout
                            { layout = model.layout
                            , newId = model.newId
                            , title = model.title
                            }
                      ]

        Result.Err msg ->
            Debug.log msg <| { model | status = SavingState } ! [ Data.Storage.saveState model.state ]


onLoadLayout : Result String Layout -> Model -> ( Model, Cmd Msg )
onLoadLayout result ({ state } as model) =
    case result of
        Result.Ok { layout, newId, title } ->
            let
                newState =
                    { state | lastLayout = title }
            in
            { model
                | state = newState
                , layout = layout
                , newId = newId
                , title = title
                , status = LayoutLoaded
            }
                ! [ Data.Storage.saveState newState ]

        Result.Err msg ->
            Debug.log msg <| { model | status = Error msg } ! []


onSaveAsLayout ({ state, layout, newId, saveAsName } as model) =
    let
        newState =
            { state | savedLayouts = Set.insert saveAsName state.savedLayouts }
    in
    { model | openPicker = NonePicker, status = SavingLayout, state = newState }
        ! [ Data.Storage.saveLayout
                { layout = layout
                , newId = newId
                , title = saveAsName
                }
          , Data.Storage.saveState newState
          ]


onNewLayout model =
    model ! []


onUndo model =
    model ! []


onRedo model =
    model ! []


onSaveAsNameChange name model =
    { model | saveAsName = name } ! []


saveState ({ state } as model) =
    let
        newState =
            { state | lastLayout = model.title }
    in
    { model | state = newState, status = SavingState } ! [ Data.Storage.saveState newState ]
