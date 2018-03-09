module LayoutElement exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Layout.Element exposing (..)
import Test exposing (..)
import View.Stylesheet exposing (Style(..))


layout : El Style var msg
layout =
    { id = 0
    , name = "el"
    , elem =
        StyListAttrElmntElmnt el
            None
            [ { name = "padding", attr = FltAttr padding 20 }
            , { name = "center", attr = Attr center }
            , { name = "verticalCenter", attr = Attr verticalCenter }
            ]
            { id = 1, name = "text", elem = StrElmnt text "Click to edit!" }
    }


bigLayout : El Style var msg
bigLayout =
    { id = 0
    , name = "el"
    , elem =
        StyListAttrElmntElmnt el
            None
            [ { name = "padding", attr = FltAttr padding 20 }
            , { name = "center", attr = Attr center }
            , { name = "verticalCenter", attr = Attr verticalCenter }
            , { name = "height", attr = LngAttr height { name = "fillPortion", lngth = IntLng fillPortion 1 } }
            ]
            { id = 1
            , name = "row"
            , elem =
                StyListAttrListElmntElmnt row
                    None
                    [ { name = "padding", attr = FltAttr padding 20 }
                    ]
                    [ { id = 20, name = "empty", elem = Elmnt empty }
                    , { id = 30, name = "spacer", elem = FltElmnt spacer 10 }
                    , { id = 40, name = "text", elem = StrElmnt text "placeholder" }
                    , { id = 50, name = "hairline", elem = StyElmnt hairline None }
                    , { id = 60
                      , name = "column"
                      , elem =
                            StyListAttrListElmntElmnt column
                                None
                                [ { name = "padding", attr = FltAttr padding 20 }
                                , { name = "width", attr = LngAttr width { name = "px", lngth = FltLng px 300 } }
                                , { name = "paddingXY", attr = FltFltAttr paddingXY 10 30 }
                                ]
                                [ { id = 70
                                  , name = "screen"
                                  , elem =
                                        ElmntElmnt screen
                                            { id = 71
                                            , name = "button"
                                            , elem =
                                                StyListAttrElmntElmnt button
                                                    None
                                                    [ { name = "padding", attr = FltAttr padding 20 }
                                                    , { name = "alignRight", attr = Attr alignRight }
                                                    ]
                                                    { id = 72, name = "italic", elem = StrElmnt italic "screen button" }
                                            }
                                  }
                                , { id = 470
                                  , name = "onRight"
                                  , elem =
                                        ListElmntElmntElmnt onRight
                                            [ { id = 850, name = "text", elem = StrElmnt text "<- don't click on him" }
                                            ]
                                            { id = 80
                                            , name = "link"
                                            , elem =
                                                StrElmntElmnt link
                                                    "placeholder"
                                                    { id = 81, name = "strike", elem = StrElmnt strike "don't click on me" }
                                            }
                                  }
                                , { id = 250
                                  , name = "when"
                                  , elem =
                                        BoolElmntElmnt when
                                            True
                                            { id = 260
                                            , name = "subheading"
                                            , elem =
                                                StyListAttrStrElmnt subheading
                                                    None
                                                    [ { name = "padding", attr = FltAttr padding 20 }
                                                    ]
                                                    "I am a subheading!"
                                            }
                                  }
                                , { id = 1070
                                  , name = "search"
                                  , elem =
                                        StyListAttrElmntElmnt search
                                            None
                                            [ { name = "height", attr = LngAttr height { name = "fill", lngth = Lng fill } }
                                            ]
                                            { id = 1071, name = "text", elem = StrElmnt text "search area" }
                                  }
                                ]
                      }
                    , { id = 1360
                      , name = "circle"
                      , elem =
                            FltStyListAttrElmntElmnt circle
                                50
                                None
                                [ { name = "class", attr = StrAttr class "circle-class" }
                                ]
                                { id = 1361, name = "text", elem = StrElmnt text "inner circle" }
                      }
                    ]
            }
    }


suite : Test
suite =
    describe "encoding and decoding"
        [ test "encoder >> decoder should be identity on small layout" <|
            \_ ->
                layout
                    |> elEncoder
                    |> Decode.decodeValue elDecoder
                    |> Expect.equal (Result.Ok layout)
        , test "encoder >> decoder should be identity on big layout" <|
            \_ ->
                bigLayout
                    |> elEncoder
                    |> Decode.decodeValue elDecoder
                    |> Expect.equal (Result.Ok bigLayout)
        ]
