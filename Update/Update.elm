module Update.Update exposing (..)

import Element exposing (empty)
import Layout.El as El
import Layout.Element exposing (El, Elid, Elem(Elmnt))
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

        OnClick id ->
            onClick id model


onAddEl : El Style Variation Msg -> Model -> ( Model, Cmd Msg )
onAddEl elem m =
    let
        debug0 =
            Debug.log "onAddEl" elem
    in
        { m
            | layout = El.insert m.selected elem m.layout
            , newId = m.newId + 2
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
    in
        if id == mouseOver then
            if id == selected then
                { model | selected = -1 } ! []
            else
                { model | selected = id } ! []
        else
            model ! []
