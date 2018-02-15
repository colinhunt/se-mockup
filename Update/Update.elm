module Update.Update exposing (..)

import Model.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAddEl el ->
            onAddEl el model

        OnMouseEnter id ->
            onMouseEnter id model

        OnMouseLeave ->
            onMouseLeave model

        OnClick id ->
            onClick


onAddEl : El -> ( Model, Cmd Msg )
onAddEl elem m =
    let
        debug0 =
            Debug.log "onAddEl" newEl
    in
        { m | layout = insertEl elem m.layout } ! []


onMouseEnter id model =
    { model | mousedOver = id :: model.mousedOver } ! []


onMouseLeave model =
    { model | mousedOver = model.mousedOver |> List.drop 1 } ! []


onClick id model =
    let
        mouseOver =
            List.head model.mousedOver |> Maybe.withDefault -1
    in
        if id == mouseOver then
            if id == model.selected then
                { model | selected = -1 } ! []
            else
                { model | selected = id } ! []
        else
            model ! []
