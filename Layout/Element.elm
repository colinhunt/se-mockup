module Layout.Element exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Attributes exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Layout.Utils exposing (fnDecoder)
import Utils exposing ((=>))
import View.Stylesheet as Sty


type alias Elid =
    Int


type alias El sty var msg =
    { id : Elid, name : String, elem : Elem sty var msg }


type alias At var msg =
    { name : String, attr : Attr var msg }


type alias Ln =
    { name : String, lngth : Lngth }


type Elem sty var msg
    = Elmnt (ElmntFn sty var msg)
    | FltElmnt (FltElmntFn sty var msg) Float
    | StrElmnt (StrElmntFn sty var msg) String
    | StyElmnt (StyElmntFn sty var msg) sty
    | ElmntElmnt (ElmntElmntFn sty var msg) (El sty var msg)
    | StrElmntElmnt (StrElmntElmntFn sty var msg) String (El sty var msg)
    | BoolElmntElmnt (BoolElmntElmntFn sty var msg) Bool (El sty var msg)
    | StyListAttrStrElmnt (StyListAttrStrElmntFn sty var msg) sty (List (At var msg)) String
    | ListElmntElmntElmnt (ListElmntElmntElmntFn sty var msg) (List (El sty var msg)) (El sty var msg)
    | StyListAttrElmntElmnt (StyListAttrElmntElmntFn sty var msg) sty (List (At var msg)) (El sty var msg)
    | FltStyListAttrElmntElmnt (FltStyListAttrElmntElmntFn sty var msg) Float sty (List (At var msg)) (El sty var msg)
    | StyListAttrListElmntElmnt (StyListAttrListElmntElmntFn sty var msg) sty (List (At var msg)) (List (El sty var msg))


{-| Case statement template:
-}



-- caseElem elem =
--     case elem of
--          Elmnt f  ->
--              Elmnt f
--          FltElmnt f flt ->
--              FltElmnt f flt
--          StrElmnt f str ->
--              StrElmnt f str
--          StyElmnt f sty ->
--              StyElmnt f sty
--          ElmntElmnt f el ->
--              ElmntElmnt f el
--          StrElmntElmnt f str el ->
--              StrElmntElmnt f str el
--          BoolElmntElmnt f bool el ->
--              BoolElmntElmnt f bool el
--          StyListAttrStrElmnt f sty attrs str ->
--              StyListAttrStrElmnt f sty attrs str
--          ListElmntElmntElmnt f els el ->
--              ListElmntElmntElmnt f els el
--          StyListAttrElmntElmnt f sty attrs el ->
--              StyListAttrElmntElmnt f sty attrs el
--          FltStyListAttrElmntElmnt f flt sty attrs el ->
--              FltStyListAttrElmntElmnt f flt sty attrs el
--          StyListAttrListElmntElmnt f sty attrs els ->
--              StyListAttrListElmntElmnt f sty attrs els


type alias ElmntFn sty var msg =
    Element sty var msg


type alias FltElmntFn sty var msg =
    Float -> Element sty var msg


type alias StrElmntFn sty var msg =
    String -> Element sty var msg


type alias StyElmntFn sty var msg =
    sty -> Element sty var msg


type alias ElmntElmntFn sty var msg =
    Element sty var msg -> Element sty var msg


type alias StrElmntElmntFn sty var msg =
    String -> Element sty var msg -> Element sty var msg


type alias BoolElmntElmntFn sty var msg =
    Bool -> Element sty var msg -> Element sty var msg


type alias StyListAttrStrElmntFn sty var msg =
    sty -> List (Attribute var msg) -> String -> Element sty var msg


type alias ListElmntElmntElmntFn sty var msg =
    List (Element sty var msg) -> Element sty var msg -> Element sty var msg


type alias StyListAttrElmntElmntFn sty var msg =
    sty -> List (Attribute var msg) -> Element sty var msg -> Element sty var msg


type alias FltStyListAttrElmntElmntFn sty var msg =
    Float -> sty -> List (Attribute var msg) -> Element sty var msg -> Element sty var msg


type alias StyListAttrListElmntElmntFn sty var msg =
    sty -> List (Attribute var msg) -> List (Element sty var msg) -> Element sty var msg


allElems : Int -> List (El Sty.Style var msg) -> List (El Sty.Style var msg)
allElems id children =
    [ { id = id, name = "empty", elem = Elmnt empty }
    , { id = id, name = "spacer", elem = FltElmnt spacer 10 }
    , { id = id, name = "text", elem = StrElmnt text "placeholder" }
    , { id = id, name = "bold", elem = StrElmnt bold "placeholder" }
    , { id = id, name = "italic", elem = StrElmnt italic "placeholder" }
    , { id = id, name = "strike", elem = StrElmnt strike "placeholder" }
    , { id = id, name = "underline", elem = StrElmnt underline "placeholder" }
    , { id = id, name = "sub", elem = StrElmnt sub "placeholder" }
    , { id = id, name = "super", elem = StrElmnt super "placeholder" }
    , { id = id, name = "hairline", elem = StyElmnt hairline Sty.None }
    , { id = id, name = "screen", elem = ElmntElmnt screen (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "node", elem = StrElmntElmnt node "placeholder" (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "link", elem = StrElmntElmnt link "placeholder" (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "newTab", elem = StrElmntElmnt newTab "placeholder" (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "download", elem = StrElmntElmnt download "placeholder" (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "when", elem = BoolElmntElmnt when False (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "subheading", elem = StyListAttrStrElmnt subheading Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] "placeholder" }
    , { id = id, name = "within", elem = ListElmntElmntElmnt within children (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "above", elem = ListElmntElmntElmnt above children (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "below", elem = ListElmntElmntElmnt below children (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "onRight", elem = ListElmntElmntElmnt onRight children (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "onLeft", elem = ListElmntElmntElmnt onLeft children (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "el", elem = StyListAttrElmntElmnt el Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "section", elem = StyListAttrElmntElmnt section Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "article", elem = StyListAttrElmntElmnt article Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "aside", elem = StyListAttrElmntElmnt aside Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "button", elem = StyListAttrElmntElmnt button Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "h1", elem = StyListAttrElmntElmnt h1 Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "h2", elem = StyListAttrElmntElmnt h2 Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "h3", elem = StyListAttrElmntElmnt h3 Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "h4", elem = StyListAttrElmntElmnt h4 Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "h5", elem = StyListAttrElmntElmnt h5 Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "h6", elem = StyListAttrElmntElmnt h6 Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "full", elem = StyListAttrElmntElmnt full Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "search", elem = StyListAttrElmntElmnt search Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "header", elem = StyListAttrElmntElmnt header Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "footer", elem = StyListAttrElmntElmnt footer Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "mainContent", elem = StyListAttrElmntElmnt mainContent Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "modal", elem = StyListAttrElmntElmnt modal Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "circle", elem = FltStyListAttrElmntElmnt circle 10 Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] (children |> List.head |> Maybe.withDefault { id = id + 1, name = "empty", elem = Elmnt empty }) }
    , { id = id, name = "textLayout", elem = StyListAttrListElmntElmnt textLayout Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] children }
    , { id = id, name = "paragraph", elem = StyListAttrListElmntElmnt paragraph Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] children }
    , { id = id, name = "row", elem = StyListAttrListElmntElmnt row Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] children }
    , { id = id, name = "column", elem = StyListAttrListElmntElmnt column Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] children }
    , { id = id, name = "wrappedRow", elem = StyListAttrListElmntElmnt wrappedRow Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] children }
    , { id = id, name = "wrappedColumn", elem = StyListAttrListElmntElmnt wrappedColumn Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] children }
    , { id = id, name = "sidebar", elem = StyListAttrListElmntElmnt sidebar Sty.None [ { name = "padding", attr = FltAttr padding 20 } ] children }
    ]


elmntFnLookup =
    Dict.fromList
        [ "empty" => empty
        ]


fltElmntFnLookup =
    Dict.fromList
        [ "spacer" => spacer
        ]


strElmntFnLookup =
    Dict.fromList
        [ "text" => text
        , "bold" => bold
        , "italic" => italic
        , "strike" => strike
        , "underline" => underline
        , "sub" => sub
        , "super" => super
        ]


styElmntFnLookup =
    Dict.fromList
        [ "hairline" => hairline
        ]


elmntElmntFnLookup =
    Dict.fromList
        [ "screen" => screen
        ]


strElmntElmntFnLookup =
    Dict.fromList
        [ "node" => node
        , "link" => link
        , "newTab" => newTab
        , "download" => download
        ]


boolElmntElmntFnLookup =
    Dict.fromList
        [ "when" => when
        ]


styListAttrStrElmntFnLookup =
    Dict.fromList
        [ "subheading" => subheading
        ]


listElmntElmntElmntFnLookup =
    Dict.fromList
        [ "within" => within
        , "above" => above
        , "below" => below
        , "onRight" => onRight
        , "onLeft" => onLeft
        ]


styListAttrElmntElmntFnLookup =
    Dict.fromList
        [ "el" => el
        , "section" => section
        , "article" => article
        , "aside" => aside
        , "button" => button
        , "h1" => h1
        , "h2" => h2
        , "h3" => h3
        , "h4" => h4
        , "h5" => h5
        , "h6" => h6
        , "full" => full
        , "search" => search
        , "header" => header
        , "footer" => footer
        , "mainContent" => mainContent
        , "modal" => modal
        ]


fltStyListAttrElmntElmntFnLookup =
    Dict.fromList
        [ "circle" => circle
        ]


styListAttrListElmntElmntFnLookup =
    Dict.fromList
        [ "textLayout" => textLayout
        , "paragraph" => paragraph
        , "row" => row
        , "column" => column
        , "wrappedRow" => wrappedRow
        , "wrappedColumn" => wrappedColumn
        , "sidebar" => sidebar
        ]


elEncoder thing =
    Encode.object
        [ "id" => Encode.int thing.id
        , "name" => Encode.string thing.name
        , "elem" => elemEncoder thing
        ]


elemEncoder { name, elem } =
    Encode.object <|
        case elem of
            Elmnt f ->
                [ "kind" => Encode.string "Elmnt"
                , "fn" => Encode.string name
                ]

            FltElmnt f flt ->
                [ "kind" => Encode.string "FltElmnt"
                , "fn" => Encode.string name
                , "flt" => Encode.float flt
                ]

            StrElmnt f str ->
                [ "kind" => Encode.string "StrElmnt"
                , "fn" => Encode.string name
                , "str" => Encode.string str
                ]

            StyElmnt f sty ->
                [ "kind" => Encode.string "StyElmnt"
                , "fn" => Encode.string name
                , "sty" => (always <| Encode.string "None") sty
                ]

            ElmntElmnt f el ->
                [ "kind" => Encode.string "ElmntElmnt"
                , "fn" => Encode.string name
                , "el" => elEncoder el
                ]

            StrElmntElmnt f str el ->
                [ "kind" => Encode.string "StrElmntElmnt"
                , "fn" => Encode.string name
                , "str" => Encode.string str
                , "el" => elEncoder el
                ]

            BoolElmntElmnt f bool el ->
                [ "kind" => Encode.string "BoolElmntElmnt"
                , "fn" => Encode.string name
                , "bool" => Encode.bool bool
                , "el" => elEncoder el
                ]

            StyListAttrStrElmnt f sty attrs str ->
                [ "kind" => Encode.string "StyListAttrStrElmnt"
                , "fn" => Encode.string name
                , "sty" => (always <| Encode.string "None") sty
                , "attrs" => Encode.list <| List.map atEncoder attrs
                , "str" => Encode.string str
                ]

            ListElmntElmntElmnt f els el ->
                [ "kind" => Encode.string "ListElmntElmntElmnt"
                , "fn" => Encode.string name
                , "els" => Encode.list <| List.map elEncoder els
                , "el" => elEncoder el
                ]

            StyListAttrElmntElmnt f sty attrs el ->
                [ "kind" => Encode.string "StyListAttrElmntElmnt"
                , "fn" => Encode.string name
                , "sty" => (always <| Encode.string "None") sty
                , "attrs" => Encode.list <| List.map atEncoder attrs
                , "el" => elEncoder el
                ]

            FltStyListAttrElmntElmnt f flt sty attrs el ->
                [ "kind" => Encode.string "FltStyListAttrElmntElmnt"
                , "fn" => Encode.string name
                , "flt" => Encode.float flt
                , "sty" => (always <| Encode.string "None") sty
                , "attrs" => Encode.list <| List.map atEncoder attrs
                , "el" => elEncoder el
                ]

            StyListAttrListElmntElmnt f sty attrs els ->
                [ "kind" => Encode.string "StyListAttrListElmntElmnt"
                , "fn" => Encode.string name
                , "sty" => (always <| Encode.string "None") sty
                , "attrs" => Encode.list <| List.map atEncoder attrs
                , "els" => Encode.list <| List.map elEncoder els
                ]



{-
   The decoder pairs must be defined in this order and the lazy
   decoding must go in the first one or we get runtime errors.

   Caused and fix as suggested by
   https://github.com/elm-lang/elm-compiler/issues/1560
-}


elemDecoder =
    Decode.oneOf
        [ Decode.map Elmnt
            (Decode.field "fn" <| fnDecoder elmntFnLookup)
        , Decode.map2 FltElmnt
            (Decode.field "fn" <| fnDecoder fltElmntFnLookup)
            (Decode.field "flt" Decode.float)
        , Decode.map2 StrElmnt
            (Decode.field "fn" <| fnDecoder strElmntFnLookup)
            (Decode.field "str" Decode.string)
        , Decode.map2 StyElmnt
            (Decode.field "fn" <| fnDecoder styElmntFnLookup)
            (Decode.field "sty" <| Decode.succeed Sty.None)
        , Decode.map2 ElmntElmnt
            (Decode.field "fn" <| fnDecoder elmntElmntFnLookup)
            (Decode.field "el" <| Decode.lazy (\_ -> elDecoder))
        , Decode.map3 StrElmntElmnt
            (Decode.field "fn" <| fnDecoder strElmntElmntFnLookup)
            (Decode.field "str" Decode.string)
            (Decode.field "el" <| Decode.lazy (\_ -> elDecoder))
        , Decode.map3 BoolElmntElmnt
            (Decode.field "fn" <| fnDecoder boolElmntElmntFnLookup)
            (Decode.field "bool" Decode.bool)
            (Decode.field "el" <| Decode.lazy (\_ -> elDecoder))
        , Decode.map4 StyListAttrStrElmnt
            (Decode.field "fn" <| fnDecoder styListAttrStrElmntFnLookup)
            (Decode.field "sty" <| Decode.succeed Sty.None)
            (Decode.field "attrs" <| Decode.list atDecoder)
            (Decode.field "str" Decode.string)
        , Decode.map3 ListElmntElmntElmnt
            (Decode.field "fn" <| fnDecoder listElmntElmntElmntFnLookup)
            (Decode.field "els" <| Decode.list <| Decode.lazy (\_ -> elDecoder))
            (Decode.field "el" <| Decode.lazy (\_ -> elDecoder))
        , Decode.map4 StyListAttrElmntElmnt
            (Decode.field "fn" <| fnDecoder styListAttrElmntElmntFnLookup)
            (Decode.field "sty" <| Decode.succeed Sty.None)
            (Decode.field "attrs" <| Decode.list atDecoder)
            (Decode.field "el" <| Decode.lazy (\_ -> elDecoder))
        , Decode.map5 FltStyListAttrElmntElmnt
            (Decode.field "fn" <| fnDecoder fltStyListAttrElmntElmntFnLookup)
            (Decode.field "flt" Decode.float)
            (Decode.field "sty" <| Decode.succeed Sty.None)
            (Decode.field "attrs" <| Decode.list atDecoder)
            (Decode.field "el" <| Decode.lazy (\_ -> elDecoder))
        , Decode.map4 StyListAttrListElmntElmnt
            (Decode.field "fn" <| fnDecoder styListAttrListElmntElmntFnLookup)
            (Decode.field "sty" <| Decode.succeed Sty.None)
            (Decode.field "attrs" <| Decode.list atDecoder)
            (Decode.field "els" <| Decode.list <| Decode.lazy (\_ -> elDecoder))
        ]


elDecoder =
    Decode.map3 El
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "elem" elemDecoder)


type Attr var msg
    = Attr (AttrFn var msg)
    | StrAttr (StrAttrFn var msg) String
    | LngAttr (LngAttrFn var msg) Ln
    | FltAttr (FltAttrFn var msg) Float
    | FltFltAttr (FltFltAttrFn var msg) Float Float


{-| Case statement template:
-}



-- caseAttr attr =
--     case attr of
--          Attr f  ->
--              Attr f
--          StrAttr f str ->
--              StrAttr f str
--          LngAttr f lng ->
--              LngAttr f lng
--          FltAttr f flt ->
--              FltAttr f flt
--          FltFltAttr f flt flt ->
--              FltFltAttr f flt flt


type alias AttrFn var msg =
    Attribute var msg


type alias StrAttrFn var msg =
    String -> Attribute var msg


type alias LngAttrFn var msg =
    Length -> Attribute var msg


type alias FltAttrFn var msg =
    Float -> Attribute var msg


type alias FltFltAttrFn var msg =
    Float -> Float -> Attribute var msg


allAttrs : List (At var msg)
allAttrs =
    [ { name = "center", attr = Attr center }
    , { name = "verticalCenter", attr = Attr verticalCenter }
    , { name = "verticalSpread", attr = Attr verticalSpread }
    , { name = "spread", attr = Attr spread }
    , { name = "alignTop", attr = Attr alignTop }
    , { name = "alignBottom", attr = Attr alignBottom }
    , { name = "alignLeft", attr = Attr alignLeft }
    , { name = "alignRight", attr = Attr alignRight }
    , { name = "hidden", attr = Attr hidden }
    , { name = "scrollbars", attr = Attr scrollbars }
    , { name = "yScrollbar", attr = Attr yScrollbar }
    , { name = "xScrollbar", attr = Attr xScrollbar }
    , { name = "clip", attr = Attr clip }
    , { name = "clipX", attr = Attr clipX }
    , { name = "clipY", attr = Attr clipY }
    , { name = "class", attr = StrAttr class "placeholder" }
    , { name = "id", attr = StrAttr id "placeholder" }
    , { name = "width", attr = LngAttr width { name = "px", lngth = FltLng px 10 } }
    , { name = "minWidth", attr = LngAttr minWidth { name = "px", lngth = FltLng px 10 } }
    , { name = "maxWidth", attr = LngAttr maxWidth { name = "px", lngth = FltLng px 10 } }
    , { name = "minHeight", attr = LngAttr minHeight { name = "px", lngth = FltLng px 10 } }
    , { name = "maxHeight", attr = LngAttr maxHeight { name = "px", lngth = FltLng px 10 } }
    , { name = "height", attr = LngAttr height { name = "px", lngth = FltLng px 10 } }
    , { name = "moveUp", attr = FltAttr moveUp 10 }
    , { name = "moveDown", attr = FltAttr moveDown 10 }
    , { name = "moveRight", attr = FltAttr moveRight 10 }
    , { name = "moveLeft", attr = FltAttr moveLeft 10 }
    , { name = "spacing", attr = FltAttr spacing 10 }
    , { name = "padding", attr = FltAttr padding 10 }
    , { name = "paddingLeft", attr = FltAttr paddingLeft 10 }
    , { name = "paddingRight", attr = FltAttr paddingRight 10 }
    , { name = "paddingTop", attr = FltAttr paddingTop 10 }
    , { name = "paddingBottom", attr = FltAttr paddingBottom 10 }
    , { name = "spacingXY", attr = FltFltAttr spacingXY 10 10 }
    , { name = "paddingXY", attr = FltFltAttr paddingXY 10 10 }
    ]


attrFnLookup =
    Dict.fromList
        [ "center" => center
        , "verticalCenter" => verticalCenter
        , "verticalSpread" => verticalSpread
        , "spread" => spread
        , "alignTop" => alignTop
        , "alignBottom" => alignBottom
        , "alignLeft" => alignLeft
        , "alignRight" => alignRight
        , "hidden" => hidden
        , "scrollbars" => scrollbars
        , "yScrollbar" => yScrollbar
        , "xScrollbar" => xScrollbar
        , "clip" => clip
        , "clipX" => clipX
        , "clipY" => clipY
        ]


strAttrFnLookup =
    Dict.fromList
        [ "class" => class
        , "id" => id
        ]


lngAttrFnLookup =
    Dict.fromList
        [ "width" => width
        , "minWidth" => minWidth
        , "maxWidth" => maxWidth
        , "minHeight" => minHeight
        , "maxHeight" => maxHeight
        , "height" => height
        ]


fltAttrFnLookup =
    Dict.fromList
        [ "moveUp" => moveUp
        , "moveDown" => moveDown
        , "moveRight" => moveRight
        , "moveLeft" => moveLeft
        , "spacing" => spacing
        , "padding" => padding
        , "paddingLeft" => paddingLeft
        , "paddingRight" => paddingRight
        , "paddingTop" => paddingTop
        , "paddingBottom" => paddingBottom
        ]


fltFltAttrFnLookup =
    Dict.fromList
        [ "spacingXY" => spacingXY
        , "paddingXY" => paddingXY
        ]


atEncoder thing =
    Encode.object
        [ "name" => Encode.string thing.name
        , "attr" => attrEncoder thing
        ]


attrEncoder { name, attr } =
    Encode.object <|
        case attr of
            Attr f ->
                [ "kind" => Encode.string "Attr"
                , "fn" => Encode.string name
                ]

            StrAttr f str ->
                [ "kind" => Encode.string "StrAttr"
                , "fn" => Encode.string name
                , "str" => Encode.string str
                ]

            LngAttr f lng ->
                [ "kind" => Encode.string "LngAttr"
                , "fn" => Encode.string name
                , "lng" => lnEncoder lng
                ]

            FltAttr f flt ->
                [ "kind" => Encode.string "FltAttr"
                , "fn" => Encode.string name
                , "flt" => Encode.float flt
                ]

            FltFltAttr f flt flt1 ->
                [ "kind" => Encode.string "FltFltAttr"
                , "fn" => Encode.string name
                , "flt" => Encode.float flt
                , "flt1" => Encode.float flt1
                ]



{-
   The decoder pairs must be defined in this order and the lazy
   decoding must go in the first one or we get runtime errors.

   Caused and fix as suggested by
   https://github.com/elm-lang/elm-compiler/issues/1560
-}


attrDecoder =
    Decode.oneOf
        [ Decode.map Attr
            (Decode.field "fn" <| fnDecoder attrFnLookup)
        , Decode.map2 StrAttr
            (Decode.field "fn" <| fnDecoder strAttrFnLookup)
            (Decode.field "str" Decode.string)
        , Decode.map2 LngAttr
            (Decode.field "fn" <| fnDecoder lngAttrFnLookup)
            (Decode.field "lng" lnDecoder)
        , Decode.map2 FltAttr
            (Decode.field "fn" <| fnDecoder fltAttrFnLookup)
            (Decode.field "flt" Decode.float)
        , Decode.map3 FltFltAttr
            (Decode.field "fn" <| fnDecoder fltFltAttrFnLookup)
            (Decode.field "flt" Decode.float)
            (Decode.field "flt1" Decode.float)
        ]


atDecoder =
    Decode.map2 At
        (Decode.field "name" Decode.string)
        (Decode.field "attr" attrDecoder)


type Lngth
    = Lng LngFn
    | FltLng FltLngFn Float
    | IntLng IntLngFn Int


{-| Case statement template:
-}



-- caseLngth lngth =
--     case lngth of
--          Lng f  ->
--              Lng f
--          FltLng f flt ->
--              FltLng f flt
--          IntLng f int ->
--              IntLng f int


type alias LngFn =
    Length


type alias FltLngFn =
    Float -> Length


type alias IntLngFn =
    Int -> Length


allLngths : List Ln
allLngths =
    [ { name = "content", lngth = Lng content }
    , { name = "fill", lngth = Lng fill }
    , { name = "px", lngth = FltLng px 10 }
    , { name = "percent", lngth = FltLng percent 10 }
    , { name = "fillPortion", lngth = IntLng fillPortion 10 }
    ]


lngFnLookup =
    Dict.fromList
        [ "content" => content
        , "fill" => fill
        ]


fltLngFnLookup =
    Dict.fromList
        [ "px" => px
        , "percent" => percent
        ]


intLngFnLookup =
    Dict.fromList
        [ "fillPortion" => fillPortion
        ]


lnEncoder thing =
    Encode.object
        [ "name" => Encode.string thing.name
        , "lngth" => lngthEncoder thing
        ]


lngthEncoder { name, lngth } =
    Encode.object <|
        case lngth of
            Lng f ->
                [ "kind" => Encode.string "Lng"
                , "fn" => Encode.string name
                ]

            FltLng f flt ->
                [ "kind" => Encode.string "FltLng"
                , "fn" => Encode.string name
                , "flt" => Encode.float flt
                ]

            IntLng f int ->
                [ "kind" => Encode.string "IntLng"
                , "fn" => Encode.string name
                , "int" => Encode.int int
                ]



{-
   The decoder pairs must be defined in this order and the lazy
   decoding must go in the first one or we get runtime errors.

   Caused and fix as suggested by
   https://github.com/elm-lang/elm-compiler/issues/1560
-}


lngthDecoder =
    Decode.oneOf
        [ Decode.map Lng
            (Decode.field "fn" <| fnDecoder lngFnLookup)
        , Decode.map2 FltLng
            (Decode.field "fn" <| fnDecoder fltLngFnLookup)
            (Decode.field "flt" Decode.float)
        , Decode.map2 IntLng
            (Decode.field "fn" <| fnDecoder intLngFnLookup)
            (Decode.field "int" Decode.int)
        ]


lnDecoder =
    Decode.map2 Ln
        (Decode.field "name" Decode.string)
        (Decode.field "lngth" lngthDecoder)
