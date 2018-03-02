module Utils exposing (..)

import Element.Events exposing (defaultOptions, onWithOptions)
import Json.Decode as Json


quote str =
    "\"" ++ str ++ "\""


onClickNoProp msg =
    onWithOptions "click" { defaultOptions | stopPropagation = True } (Json.succeed msg)
