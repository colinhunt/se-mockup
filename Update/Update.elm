module Update.Update exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Utils as U
import View.Stylesheet exposing (..)
import Model.Model exposing (..)
import Model.Utils as M
import Update.El exposing (..)


type alias Updater =
    Model -> ( Model, Cmd Msg )


update : Msg -> Updater
update (Fun f) model =
    f model


onAddEl : El -> Updater
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
