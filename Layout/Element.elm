module Layout.Element exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
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
