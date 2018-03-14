port module Data.Storage exposing (loadState, onLoad, saveLayout)

import Data.Ports
import Dict
import Json.Bidirectional exposing (..)
import Layout.Element exposing (El, Elid, elDecoder, elEncoder)
import Model.Types exposing (Layout, Msg(..))
import View.Stylesheet exposing (Style, Variation)


type alias State msg =
    { layout : El Style Variation msg
    , newId : Elid
    }


type StorageKey
    = StorageKey String


saveLayout : Layout -> Cmd msg
saveLayout layout =
    Data.Ports.save <| encodeValue (keyedValue (StorageKey layout.title) layoutCoder) layout


loadState : Cmd msg
loadState =
    Data.Ports.load [ "state" ]


keyToString : StorageKey -> String
keyToString (StorageKey key) =
    key


keyedValue : StorageKey -> Coder a -> Coder a
keyedValue key coder =
    at [ keyToString key ] coder


layoutCoder : Coder Layout
layoutCoder =
    object Layout
        |> withField "layout" .layout elCoder
        |> withField "newId" .newId elidCoder
        |> withField "title" .title string


elCoder : Coder (El Style Variation msg)
elCoder =
    custom elEncoder elDecoder


elidCoder : Coder Elid
elidCoder =
    int


onLoad : Sub Msg
onLoad =
    Data.Ports.onLoad <|
        \json ->
            let
                items =
                    decodeValue (dict value) json
                        |> Result.withDefault Dict.empty
                        |> Dict.toList
            in
            case items of
                ( key, value ) :: rest ->
                    case key of
                        "state" ->
                            decodeState value

                        _ ->
                            NoneMsg |> Debug.log ("Storage.onLoad: unrecognized key " ++ key)

                [] ->
                    NoneMsg
                        |> Debug.log
                            ("Storage.onLoad could not find anything to load in "
                                ++ toString json
                            )


decodeState : Value -> Msg
decodeState json =
    decodeValue layoutCoder json
        |> OnLoadState
