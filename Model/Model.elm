module Model.Model exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import View.Stylesheet exposing (..)
import Model.Element exposing (..)


type Msg
    = Fun (Model -> ( Model, Cmd Msg ))


type alias Model =
    { layout : El
    , mousedOver : List Elid
    , selected : Elid
    , newId : Elid
    }


initModel =
    { layout =
        { id = 9
        , el =
            SAEE
                el
                Elmnt
                [ LA height (L fill), LA width (L fill) ]
                { id = 0
                , el =
                    (SALE
                        row
                        Row
                        [ A center
                        , A verticalCenter
                        , FA padding 20
                        , FA spacing 10
                        ]
                        [ { id = 1, el = StrE text "One" }
                        , { id = 2, el = StrE text "Two" }
                        , { id = 3
                          , el =
                                SALE
                                    column
                                    Column
                                    []
                                    [ { id = 4, el = StrE text "Three" }
                                    , { id = 5, el = StrE text "Four" }
                                    ]
                          }
                        , { id = 6, el = StrE text "Five" }
                        ]
                    )
                }
        }
    , mousedOver = []
    , selected = -1
    , newId = 42
    }
