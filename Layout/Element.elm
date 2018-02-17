module Layout.Element exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import View.Stylesheet as Sty


type alias Elid =
    Int


type alias El sty var msg =
    { id : Elid, name : String, elem : Elem sty var msg }


type Elem sty var msg
    = Elmnt (ElmntFn sty var msg)
    | FltElmnt (FltElmntFn sty var msg) Float
    | StrElmnt (StrElmntFn sty var msg) String
    | StyElmnt (StyElmntFn sty var msg) sty
    | ElmntElmnt (ElmntElmntFn sty var msg) (El sty var msg)
    | StrElmntElmnt (StrElmntElmntFn sty var msg) String (El sty var msg)
    | BoolElmntElmnt (BoolElmntElmntFn sty var msg) Bool (El sty var msg)
    | StyListAttrStrElmnt (StyListAttrStrElmntFn sty var msg) sty (List (Attr var msg)) String
    | ListElmntElmntElmnt (ListElmntElmntElmntFn sty var msg) (List (El sty var msg)) (El sty var msg)
    | StyListAttrElmntElmnt (StyListAttrElmntElmntFn sty var msg) sty (List (Attr var msg)) (El sty var msg)
    | FltStyListAttrElmntElmnt (FltStyListAttrElmntElmntFn sty var msg) Float sty (List (Attr var msg)) (El sty var msg)
    | StyListAttrListElmntElmnt (StyListAttrListElmntElmntFn sty var msg) sty (List (Attr var msg)) (List (El sty var msg))


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


allElems id =
    [ { id = id, name = "empty", elem = Elmnt empty }
    , { id = id, name = "spacer", elem = FltElmnt spacer 10 }
    , { id = id, name = "text", elem = StrElmnt text "placeholder" }
    , { id = id, name = "bold", elem = StrElmnt bold "placeholder" }
    , { id = id, name = "italic", elem = StrElmnt italic "placeholder" }
    , { id = id, name = "strike", elem = StrElmnt strike "placeholder" }
    , { id = id, name = "underline", elem = StrElmnt underline "placeholder" }
    , { id = id, name = "sub", elem = StrElmnt sub "placeholder" }
    , { id = id, name = "super", elem = StrElmnt super "placeholder" }
    , { id = id, name = "hairline", elem = StyElmnt hairline Sty.Elmnt }
    , { id = id, name = "screen", elem = ElmntElmnt screen { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "node", elem = StrElmntElmnt node "placeholder" { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "link", elem = StrElmntElmnt link "placeholder" { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "newTab", elem = StrElmntElmnt newTab "placeholder" { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "download", elem = StrElmntElmnt download "placeholder" { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "when", elem = BoolElmntElmnt when False { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "subheading", elem = StyListAttrStrElmnt subheading Sty.Elmnt [ FltAttr padding 20 ] "placeholder" }
    , { id = id, name = "within", elem = ListElmntElmntElmnt within [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "above", elem = ListElmntElmntElmnt above [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "below", elem = ListElmntElmntElmnt below [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "onRight", elem = ListElmntElmntElmnt onRight [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "onLeft", elem = ListElmntElmntElmnt onLeft [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "el", elem = StyListAttrElmntElmnt el Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "section", elem = StyListAttrElmntElmnt section Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "article", elem = StyListAttrElmntElmnt article Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "aside", elem = StyListAttrElmntElmnt aside Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "button", elem = StyListAttrElmntElmnt button Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "h1", elem = StyListAttrElmntElmnt h1 Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "h2", elem = StyListAttrElmntElmnt h2 Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "h3", elem = StyListAttrElmntElmnt h3 Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "h4", elem = StyListAttrElmntElmnt h4 Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "h5", elem = StyListAttrElmntElmnt h5 Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "h6", elem = StyListAttrElmntElmnt h6 Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "full", elem = StyListAttrElmntElmnt full Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "search", elem = StyListAttrElmntElmnt search Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "header", elem = StyListAttrElmntElmnt header Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "footer", elem = StyListAttrElmntElmnt footer Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "mainContent", elem = StyListAttrElmntElmnt mainContent Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "modal", elem = StyListAttrElmntElmnt modal Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "circle", elem = FltStyListAttrElmntElmnt circle 10 Sty.Elmnt [ FltAttr padding 20 ] { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } }
    , { id = id, name = "textLayout", elem = StyListAttrListElmntElmnt textLayout Sty.Elmnt [ FltAttr padding 20 ] [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] }
    , { id = id, name = "paragraph", elem = StyListAttrListElmntElmnt paragraph Sty.Elmnt [ FltAttr padding 20 ] [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] }
    , { id = id, name = "row", elem = StyListAttrListElmntElmnt row Sty.Elmnt [ FltAttr padding 20 ] [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] }
    , { id = id, name = "column", elem = StyListAttrListElmntElmnt column Sty.Elmnt [ FltAttr padding 20 ] [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] }
    , { id = id, name = "wrappedRow", elem = StyListAttrListElmntElmnt wrappedRow Sty.Elmnt [ FltAttr padding 20 ] [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] }
    , { id = id, name = "wrappedColumn", elem = StyListAttrListElmntElmnt wrappedColumn Sty.Elmnt [ FltAttr padding 20 ] [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] }
    , { id = id, name = "sidebar", elem = StyListAttrListElmntElmnt sidebar Sty.Elmnt [ FltAttr padding 20 ] [ { id = id + 1, name = "text", elem = StrElmnt text "Click to edit" } ] }
    ]


type Attr var msg
    = Attr (AttrFn var msg)
    | StrAttr (StrAttrFn var msg) String
    | LngAttr (LngAttrFn var msg) Lngth
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


allAttrs id =
    [ { id = id, name = "center", elem = Attr center }
    , { id = id, name = "verticalCenter", elem = Attr verticalCenter }
    , { id = id, name = "verticalSpread", elem = Attr verticalSpread }
    , { id = id, name = "spread", elem = Attr spread }
    , { id = id, name = "alignTop", elem = Attr alignTop }
    , { id = id, name = "alignBottom", elem = Attr alignBottom }
    , { id = id, name = "alignLeft", elem = Attr alignLeft }
    , { id = id, name = "alignRight", elem = Attr alignRight }
    , { id = id, name = "hidden", elem = Attr hidden }
    , { id = id, name = "scrollbars", elem = Attr scrollbars }
    , { id = id, name = "yScrollbar", elem = Attr yScrollbar }
    , { id = id, name = "xScrollbar", elem = Attr xScrollbar }
    , { id = id, name = "clip", elem = Attr clip }
    , { id = id, name = "clipX", elem = Attr clipX }
    , { id = id, name = "clipY", elem = Attr clipY }
    , { id = id, name = "class", elem = StrAttr class "placeholder" }
    , { id = id, name = "id", elem = StrAttr id "placeholder" }
    , { id = id, name = "width", elem = LngAttr width (FltLng px 10) }
    , { id = id, name = "minWidth", elem = LngAttr minWidth (FltLng px 10) }
    , { id = id, name = "maxWidth", elem = LngAttr maxWidth (FltLng px 10) }
    , { id = id, name = "minHeight", elem = LngAttr minHeight (FltLng px 10) }
    , { id = id, name = "maxHeight", elem = LngAttr maxHeight (FltLng px 10) }
    , { id = id, name = "height", elem = LngAttr height (FltLng px 10) }
    , { id = id, name = "moveUp", elem = FltAttr moveUp 10 }
    , { id = id, name = "moveDown", elem = FltAttr moveDown 10 }
    , { id = id, name = "moveRight", elem = FltAttr moveRight 10 }
    , { id = id, name = "moveLeft", elem = FltAttr moveLeft 10 }
    , { id = id, name = "spacing", elem = FltAttr spacing 10 }
    , { id = id, name = "padding", elem = FltAttr padding 10 }
    , { id = id, name = "paddingLeft", elem = FltAttr paddingLeft 10 }
    , { id = id, name = "paddingRight", elem = FltAttr paddingRight 10 }
    , { id = id, name = "paddingTop", elem = FltAttr paddingTop 10 }
    , { id = id, name = "paddingBottom", elem = FltAttr paddingBottom 10 }
    , { id = id, name = "spacingXY", elem = FltFltAttr spacingXY 10 10 }
    , { id = id, name = "paddingXY", elem = FltFltAttr paddingXY 10 10 }
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


allLngths id =
    [ { id = id, name = "content", elem = Lng content }
    , { id = id, name = "fill", elem = Lng fill }
    , { id = id, name = "px", elem = FltLng px 10 }
    , { id = id, name = "percent", elem = FltLng percent 10 }
    , { id = id, name = "fillPortion", elem = IntLng fillPortion 10 }
    ]
