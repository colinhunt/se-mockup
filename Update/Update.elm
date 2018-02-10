module Update.Update exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Utils as U
import View.Stylesheet exposing (..)
import Model.Model exposing (..)
import Model.Utils as M


type alias Updater =
    Model -> ( Model, Cmd Msg )


update : Msg -> Updater
update (Fun f) model =
    f model


onAddEl : El -> Updater
onAddEl newEl m =
    let
        debug0 =
            Debug.log "onAddEl" newEl

        xform : El -> El
        xform e =
            let
                debug1 =
                    Debug.log "xform" e
            in
                case e.el of
                    StrEE f str el ->
                        Debug.log "1" <| El e.id <| StrEE f str newEl

                    SAEE f style attrs el ->
                        Debug.log "2" <| El e.id <| SAEE f style attrs newEl

                    SALE f style attrs els ->
                        Debug.log "3" <| El e.id <| SALE f style attrs <| els ++ [ newEl ]

                    EE f el ->
                        Debug.log "4" <| El e.id <| EE f newEl

                    E f ->
                        Debug.log "5" <| e

                    StrE f str ->
                        Debug.log "6" <| e

                    SE f style ->
                        Debug.log "7" <| e

                    SAStrE f style attrs str ->
                        Debug.log "8" <| e

        getEls : El -> List El
        getEls e =
            let
                debug1 =
                    Debug.log "getEls" e
            in
                case e.el of
                    StrEE f str el ->
                        Debug.log "1" [ el ]

                    SAEE f style attrs el ->
                        Debug.log "2" [ el ]

                    EE f el ->
                        Debug.log "3" [ el ]

                    SALE f style attrs els ->
                        Debug.log "4" els

                    E f ->
                        Debug.log "5" []

                    StrE f str ->
                        Debug.log "6" []

                    SE f style ->
                        Debug.log "7" []

                    SAStrE f style attrs str ->
                        Debug.log "8" []

        setEls : List El -> El -> El
        setEls es e =
            let
                first =
                    List.head es

                debug1 =
                    Debug.log "setEls" ( es, e )
            in
                case e.el of
                    StrEE f str el ->
                        Debug.log "1" <| El e.id <| StrEE f str <| Maybe.withDefault el first

                    SAEE f style attrs el ->
                        Debug.log "2" <| El e.id <| SAEE f style attrs <| Maybe.withDefault el first

                    SALE f style attrs els ->
                        Debug.log "3" <| El e.id <| SALE f style attrs es

                    EE f el ->
                        Debug.log "4" <| El e.id <| EE f <| Maybe.withDefault el first

                    E f ->
                        Debug.log "5" e

                    StrE f str ->
                        Debug.log "6" e

                    SE f style ->
                        Debug.log "7" e

                    SAStrE f style attrs str ->
                        Debug.log "8" e

        layout_ =
            M.treeMap
                getEls
                setEls
                (U.when (.id >> (==) m.selected) xform)
                m.layout
    in
        { m | layout = layout_ } ! []
onAddEl : Updater
onAddEl model =
    onAddEl
        (El model.newId <|
            SAEE el Elmnt [] <|
                El (model.newId + 1) <|
                    StrE text "El"
        )
        { model | newId = model.newId + 2 }


onAddCol model =
    model ! []


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
