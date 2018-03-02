module Utils exposing (..)

import Element.Events exposing (defaultOptions, onWithOptions)
import Json.Decode as Json


quote str =
    "\"" ++ str ++ "\""


onClickNoProp msg =
    onWithOptions "click" { defaultOptions | stopPropagation = True } (Json.succeed msg)


manualOrdering : List String -> ({ b | name : String } -> { d | name : String } -> Order)
manualOrdering ordering =
    let
        order ordering a b =
            let
                mbFirst =
                    List.head ordering

                rest =
                    List.drop 1 ordering
            in
            case mbFirst of
                Just first ->
                    if a.name == first then
                        LT
                    else if b.name == first then
                        GT
                    else
                        order rest a b

                Nothing ->
                    EQ
    in
    order ordering
