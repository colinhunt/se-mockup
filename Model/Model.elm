module Model.Model exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import View.Stylesheet exposing (..)


type Msg
    = Fun (Model -> ( Model, Cmd Msg ))


type alias Elid =
    Int


type alias El =
    { id : Elid, el : Elem }


type Elem
    = E Efn
    | StrE StrEfn String
    | StrEE StrEEfn String El
    | SE SEfn Style
    | SAEE SAEEfn Style (List Attr) El
    | SALE SALEfn Style (List Attr) (List El)
    | SAStrE SAStrEfn Style (List Attr) String
    | EE EEfn El


type Attr
    = A Afn
    | LA LAfn Lngth
    | FA FAfn Float
    | FFA FFAfn Float Float


type Lngth
    = L Lfn
    | FL FLfn Float
    | IL ILfn Int



-- Element function types


type alias Efn =
    Element Style Variation Msg


type alias StrEfn =
    String
    -> Element Style Variation Msg


type alias StrEEfn =
    String
    -> Element Style Variation Msg
    -> Element Style Variation Msg


type alias SEfn =
    Style
    -> Element Style Variation Msg


type alias SAEEfn =
    Style
    -> List (Attribute Variation Msg)
    -> Element Style Variation Msg
    -> Element Style Variation Msg


type alias SALEfn =
    Style
    -> List (Attribute Variation Msg)
    -> List (Element Style Variation Msg)
    -> Element Style Variation Msg


type alias SAStrEfn =
    Style
    -> List (Attribute Variation Msg)
    -> String
    -> Element Style Variation Msg


type alias EEfn =
    Element Style Variation Msg
    -> Element Style Variation Msg



-- Attribute function types


type alias Afn =
    Attribute Variation Msg


type alias LAfn =
    Length -> Attribute Variation Msg


type alias FAfn =
    Float -> Attribute Variation Msg


type alias FFAfn =
    Float -> Float -> Attribute Variation Msg



-- Length function types


type alias Lfn =
    Length


type alias FLfn =
    Float -> Length


type alias ILfn =
    Int -> Length


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
