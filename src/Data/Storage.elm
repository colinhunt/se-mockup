port module Data.Storage
    exposing
        ( loadLayout
        , loadState
        , onLoad
        , saveLayout
        , saveState
        )

import Data.Ports
import Dict
import Json.Bidirectional exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Layout.Element exposing (El, Elid, elDecoder, elEncoder)
import Model.Types exposing (Layout, Msg(..), State)
import Set
import Utils exposing ((=>))
import View.Stylesheet exposing (Style, Variation)


type StorageKey
    = StorageKey String


saveLayout : Layout -> Cmd msg
saveLayout layout =
    Data.Ports.save <| encodeValue (keyedValue (StorageKey <| "layout/" ++ layout.title) layoutCoder) layout


saveState : State -> Cmd msg
saveState state =
    Data.Ports.save <| encodeValue (keyedValue (StorageKey <| "state/state") stateCoder) state


loadState : Cmd msg
loadState =
    Data.Ports.load [ "state/state" ]


loadLayout : String -> Cmd msg
loadLayout title =
    Data.Ports.load [ "layout/" ++ title ]



--loadLastLayout : Cmd msg
--loadLastLayout =
--    Task.perform Data.Ports.load [ "state" ]


keyToString : StorageKey -> String
keyToString (StorageKey key) =
    key


keyedValue : StorageKey -> Coder a -> Coder a
keyedValue key coder =
    at [ keyToString key ] coder


stateEncoder : State -> Value
stateEncoder state =
    Encode.object
        [ "savedLayouts" => Encode.list <| List.map Encode.string <| Set.toList state.savedLayouts
        , "lastLayout" => Encode.string state.lastLayout
        ]


stateDecoder : Decode.Decoder State
stateDecoder =
    Decode.map2 State
        (Decode.field "savedLayouts" <| Decode.map Set.fromList <| Decode.list Decode.string)
        (Decode.field "lastLayout" Decode.string)


stateCoder : Coder State
stateCoder =
    custom stateEncoder stateDecoder


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
                    case String.split "/" key of
                        kind :: id ->
                            case kind of
                                "state" ->
                                    decodeState value

                                "layout" ->
                                    decodeLayout value

                                _ ->
                                    NoneMsg |> Debug.log ("Storage.onLoad: unrecognized key " ++ key)

                        [] ->
                            NoneMsg |> Debug.log ("Storage.onLoad: invalid key " ++ key)

                [] ->
                    SaveState
                        |> Debug.log
                            ("Storage.onLoad could not find anything to load in "
                                ++ toString json
                            )


decodeState : Value -> Msg
decodeState json =
    decodeValue stateCoder json
        |> OnStorageLoadState


decodeLayout : Value -> Msg
decodeLayout json =
    decodeValue layoutCoder json
        |> OnStorageLoadLayout
