module LnJson exposing (..)

import Element.Attributes exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString, decodeValue)
import Json.Encode exposing (encode)
import Layout.Element exposing (..)
import Layout.Ln
import Test exposing (..)


lnInput : Ln
lnInput =
    { name = "px", lngth = FltLng px 200 }


lnJsonStr =
    """
    {
        "name": "px",
        "lngth": {
            "kind": "FltLng",
            "fn": "px",
            "flt": 200
        }
    }"""


stripWhitespace =
    String.filter (\c -> not <| List.member c [ ' ', '\n' ])


suite : Test
suite =
    describe "ln json"
        [ test "should encode to expected json string" <|
            \_ ->
                lnInput
                    |> Layout.Ln.encoder
                    |> encode 0
                    |> Expect.equal (stripWhitespace lnJsonStr)
        , test "should decode json string to ln type" <|
            \_ ->
                lnJsonStr
                    |> decodeString Layout.Ln.decoder
                    |> Expect.equal (Result.Ok lnInput)
        , test "encode >> decode should be identity" <|
            \_ ->
                lnInput
                    |> Layout.Ln.encoder
                    |> decodeValue Layout.Ln.decoder
                    |> Expect.equal (Result.Ok lnInput)
        ]
