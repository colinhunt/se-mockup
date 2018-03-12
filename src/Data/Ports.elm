port module Data.Ports exposing (..)

import Json.Decode exposing (Value)


port save : Value -> Cmd msg


port load : List String -> Cmd msg


port onLoad : (Value -> msg) -> Sub msg
