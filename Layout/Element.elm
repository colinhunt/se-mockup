module Layout.Element exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)


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


newEmpty : Elem sty var msg
newEmpty =
    Elmnt empty


newSpacer : Float -> Elem sty var msg
newSpacer =
    FltElmnt spacer


newText : String -> Elem sty var msg
newText =
    StrElmnt text


newBold : String -> Elem sty var msg
newBold =
    StrElmnt bold


newItalic : String -> Elem sty var msg
newItalic =
    StrElmnt italic


newStrike : String -> Elem sty var msg
newStrike =
    StrElmnt strike


newUnderline : String -> Elem sty var msg
newUnderline =
    StrElmnt underline


newSub : String -> Elem sty var msg
newSub =
    StrElmnt sub


newSuper : String -> Elem sty var msg
newSuper =
    StrElmnt super


newHairline : sty -> Elem sty var msg
newHairline style =
    StyElmnt hairline style


newScreen : El sty var msg -> Elem sty var msg
newScreen el =
    ElmntElmnt screen el


newNode : String -> El sty var msg -> Elem sty var msg
newNode str =
    StrElmntElmnt node str


newLink : String -> El sty var msg -> Elem sty var msg
newLink src el =
    StrElmntElmnt link src el


newNewtab : String -> El sty var msg -> Elem sty var msg
newNewtab src el =
    StrElmntElmnt newTab src el


newDownload : String -> El sty var msg -> Elem sty var msg
newDownload src el =
    StrElmntElmnt download src el


newWhen : Bool -> El sty var msg -> Elem sty var msg
newWhen bool elm =
    BoolElmntElmnt when bool elm


newSubheading : sty -> List (Attr var msg) -> String -> Elem sty var msg
newSubheading style attrs str =
    StyListAttrStrElmnt subheading style attrs str


newWithin : List (El sty var msg) -> El sty var msg -> Elem sty var msg
newWithin nearbys parent =
    ListElmntElmntElmnt within nearbys parent


newAbove : List (El sty var msg) -> El sty var msg -> Elem sty var msg
newAbove nearbys parent =
    ListElmntElmntElmnt above nearbys parent


newBelow : List (El sty var msg) -> El sty var msg -> Elem sty var msg
newBelow nearbys parent =
    ListElmntElmntElmnt below nearbys parent


newOnright : List (El sty var msg) -> El sty var msg -> Elem sty var msg
newOnright nearbys parent =
    ListElmntElmntElmnt onRight nearbys parent


newOnleft : List (El sty var msg) -> El sty var msg -> Elem sty var msg
newOnleft nearbys parent =
    ListElmntElmntElmnt onLeft nearbys parent


newEl : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newEl style attrs child =
    StyListAttrElmntElmnt el style attrs child


newSection : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newSection style attrs child =
    StyListAttrElmntElmnt section style attrs child


newArticle : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newArticle style attrs child =
    StyListAttrElmntElmnt article style attrs child


newAside : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newAside style attrs child =
    StyListAttrElmntElmnt aside style attrs child


newButton : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newButton style attrs child =
    StyListAttrElmntElmnt button style attrs child


newH1 : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newH1 style attrs child =
    StyListAttrElmntElmnt h1 style attrs child


newH2 : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newH2 style attrs child =
    StyListAttrElmntElmnt h2 style attrs child


newH3 : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newH3 style attrs child =
    StyListAttrElmntElmnt h3 style attrs child


newH4 : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newH4 style attrs child =
    StyListAttrElmntElmnt h4 style attrs child


newH5 : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newH5 style attrs child =
    StyListAttrElmntElmnt h5 style attrs child


newH6 : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newH6 style attrs child =
    StyListAttrElmntElmnt h6 style attrs child


newFull : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newFull elem attrs child =
    StyListAttrElmntElmnt full elem attrs child


newSearch : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newSearch style attrs child =
    StyListAttrElmntElmnt search style attrs child


newHeader : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newHeader style attrs child =
    StyListAttrElmntElmnt header style attrs child


newFooter : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newFooter style attrs child =
    StyListAttrElmntElmnt footer style attrs child


newMaincontent : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newMaincontent style attrs child =
    StyListAttrElmntElmnt mainContent style attrs child


newModal : sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newModal style attrs child =
    StyListAttrElmntElmnt modal style attrs child


newCircle : Float -> sty -> List (Attr var msg) -> El sty var msg -> Elem sty var msg
newCircle radius style attrs child =
    FltStyListAttrElmntElmnt circle radius style attrs child


newTextlayout : sty -> List (Attr var msg) -> List (El sty var msg) -> Elem sty var msg
newTextlayout style attrs children =
    StyListAttrListElmntElmnt textLayout style attrs children


newParagraph : sty -> List (Attr var msg) -> List (El sty var msg) -> Elem sty var msg
newParagraph style attrs children =
    StyListAttrListElmntElmnt paragraph style attrs children


newRow : sty -> List (Attr var msg) -> List (El sty var msg) -> Elem sty var msg
newRow style attrs children =
    StyListAttrListElmntElmnt row style attrs children


newColumn : sty -> List (Attr var msg) -> List (El sty var msg) -> Elem sty var msg
newColumn style attrs children =
    StyListAttrListElmntElmnt column style attrs children


newWrappedrow : sty -> List (Attr var msg) -> List (El sty var msg) -> Elem sty var msg
newWrappedrow style attrs children =
    StyListAttrListElmntElmnt wrappedRow style attrs children


newWrappedcolumn : sty -> List (Attr var msg) -> List (El sty var msg) -> Elem sty var msg
newWrappedcolumn style attrs children =
    StyListAttrListElmntElmnt wrappedColumn style attrs children


newSidebar : sty -> List (Attr var msg) -> List (El sty var msg) -> Elem sty var msg
newSidebar style attrs children =
    StyListAttrListElmntElmnt sidebar style attrs children


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


newCenter : Attr var msg
newCenter =
    Attr center


newVerticalcenter : Attr var msg
newVerticalcenter =
    Attr verticalCenter


newVerticalspread : Attr var msg
newVerticalspread =
    Attr verticalSpread


newSpread : Attr var msg
newSpread =
    Attr spread


newAligntop : Attr var msg
newAligntop =
    Attr alignTop


newAlignbottom : Attr var msg
newAlignbottom =
    Attr alignBottom


newAlignleft : Attr var msg
newAlignleft =
    Attr alignLeft


newAlignright : Attr var msg
newAlignright =
    Attr alignRight


newHidden : Attr var msg
newHidden =
    Attr hidden


newScrollbars : Attr var msg
newScrollbars =
    Attr scrollbars


newYscrollbar : Attr var msg
newYscrollbar =
    Attr yScrollbar


newXscrollbar : Attr var msg
newXscrollbar =
    Attr xScrollbar


newClip : Attr var msg
newClip =
    Attr clip


newClipx : Attr var msg
newClipx =
    Attr clipX


newClipy : Attr var msg
newClipy =
    Attr clipY


newClass : String -> Attr var msg
newClass cls =
    StrAttr class cls


newId : String -> Attr var msg
newId str =
    StrAttr id str


newWidth : Lngth -> Attr var msg
newWidth =
    LngAttr width


newMinwidth : Lngth -> Attr var msg
newMinwidth len =
    LngAttr minWidth len


newMaxwidth : Lngth -> Attr var msg
newMaxwidth len =
    LngAttr maxWidth len


newMinheight : Lngth -> Attr var msg
newMinheight len =
    LngAttr minHeight len


newMaxheight : Lngth -> Attr var msg
newMaxheight len =
    LngAttr maxHeight len


newHeight : Lngth -> Attr var msg
newHeight =
    LngAttr height


newMoveup : Float -> Attr var msg
newMoveup y =
    FltAttr moveUp y


newMovedown : Float -> Attr var msg
newMovedown y =
    FltAttr moveDown y


newMoveright : Float -> Attr var msg
newMoveright x =
    FltAttr moveRight x


newMoveleft : Float -> Attr var msg
newMoveleft x =
    FltAttr moveLeft x


newSpacing : Float -> Attr var msg
newSpacing x =
    FltAttr spacing x


newPadding : Float -> Attr var msg
newPadding x =
    FltAttr padding x


newPaddingleft : Float -> Attr var msg
newPaddingleft x =
    FltAttr paddingLeft x


newPaddingright : Float -> Attr var msg
newPaddingright x =
    FltAttr paddingRight x


newPaddingtop : Float -> Attr var msg
newPaddingtop x =
    FltAttr paddingTop x


newPaddingbottom : Float -> Attr var msg
newPaddingbottom x =
    FltAttr paddingBottom x


newSpacingxy : Float -> Float -> Attr var msg
newSpacingxy =
    FltFltAttr spacingXY


newPaddingxy : Float -> Float -> Attr var msg
newPaddingxy x y =
    FltFltAttr paddingXY x y


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


newContent : Lngth
newContent =
    Lng content


newFill : Lngth
newFill =
    Lng fill


newPx : Float -> Lngth
newPx =
    FltLng px


newPercent : Float -> Lngth
newPercent =
    FltLng percent


newFillportion : Int -> Lngth
newFillportion =
    IntLng fillPortion
