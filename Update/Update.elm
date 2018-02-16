module Update.Update exposing (..)

import Layout.El as El
import Layout.Element exposing (El, newEmpty)
import Model.Model exposing (..)
import View.Stylesheet exposing (Style, Variation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAddEl el ->
            onAddEl el model

        OnMouseEnter id ->
            onMouseEnter id model

        OnMouseLeave ->
            onMouseLeave model

        OnClick el ->
            onClick el model


onAddEl : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onAddEl elem m =
    let
        debug0 =
            Debug.log "onAddEl" elem
    in
        { m | layout = El.insert elem m.layout } ! []


onMouseEnter id model =
    { model | mousedOver = id :: model.mousedOver } ! []


onMouseLeave model =
    { model | mousedOver = model.mousedOver |> List.drop 1 } ! []


onClick : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onClick el model =
    let
        mouseOver =
            List.head model.mousedOver |> Maybe.withDefault -1

        selected =
            model.selected |> Maybe.withDefault { id = -1, name = "null", elem = newEmpty }
    in
        if el.id == mouseOver then
            if el.id == selected.id then
                { model | selected = Nothing } ! []
            else
                { model | selected = Just el } ! []
        else
            model ! []
