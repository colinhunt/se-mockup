module Elem exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import View.Stylesheet exposing (..)


type alias Elid =
    Int


type alias El =
    { id : Elid, el : Elem }


type Msg
    = Msg


type Elem
    = Elmnt ElmntFn
    | FltElmnt FltElmntFn Float
    | StrElmnt StrElmntFn String
    | StyElmnt StyElmntFn Style
    | ElmntElmnt ElmntElmntFn El
    | StrElmntElmnt StrElmntElmntFn String El
    | BoolElmntElmnt BoolElmntElmntFn Bool El
    | ListElmntElmntElmnt ListElmntElmntElmntFn (List El) El
    | StyListAttrStrElmnt StyListAttrStrElmntFn Style (List Attr) String
    | StyListAttrElmntElmnt StyListAttrElmntElmntFn Style (List Attr) El
    | FltStyListAttrElmntElmnt FltStyListAttrElmntElmntFn Float Style (List Attr) El
    | StyListAttrListElmntElmnt StyListAttrListElmntElmntFn Style (List Attr) (List El)


{-| Case statement template:
-}
caseElem elem =
    case elem of
        Elmnt f ->
            elem

        FltElmnt f flt0 ->
            elem

        StrElmnt f str0 ->
            elem

        StyElmnt f sty0 ->
            elem

        ElmntElmnt f el0 ->
            elem

        StrElmntElmnt f str0 el1 ->
            elem

        BoolElmntElmnt f bool0 el1 ->
            elem

        ListElmntElmntElmnt f els0 el1 ->
            elem

        StyListAttrStrElmnt f sty0 attrs1 str2 ->
            elem

        StyListAttrElmntElmnt f sty0 attrs1 el2 ->
            elem

        FltStyListAttrElmntElmnt f flt0 sty1 attrs2 el3 ->
            elem

        StyListAttrListElmntElmnt f sty0 attrs1 els2 ->
            elem


type alias ElmntFn =
    Element Style Variation Msg


type alias FltElmntFn =
    Float -> Element Style Variation Msg


type alias StrElmntFn =
    String -> Element Style Variation Msg


type alias StyElmntFn =
    Style -> Element Style Variation Msg


type alias ElmntElmntFn =
    Element Style Variation Msg -> Element Style Variation Msg


type alias StrElmntElmntFn =
    String -> Element Style Variation Msg -> Element Style Variation Msg


type alias BoolElmntElmntFn =
    Bool -> Element Style Variation Msg -> Element Style Variation Msg


type alias ListElmntElmntElmntFn =
    List (Element Style Variation Msg) -> Element Style Variation Msg -> Element Style Variation Msg


type alias StyListAttrStrElmntFn =
    Style -> List (Attribute Variation Msg) -> String -> Element Style Variation Msg


type alias StyListAttrElmntElmntFn =
    Style -> List (Attribute Variation Msg) -> Element Style Variation Msg -> Element Style Variation Msg


type alias FltStyListAttrElmntElmntFn =
    Float -> Style -> List (Attribute Variation Msg) -> Element Style Variation Msg -> Element Style Variation Msg


type alias StyListAttrListElmntElmntFn =
    Style -> List (Attribute Variation Msg) -> List (Element Style Variation Msg) -> Element Style Variation Msg


newEmpty : Elem
newEmpty =
    Elmnt empty


newSpacer : Float -> Elem
newSpacer =
    FltElmnt spacer


newText : String -> Elem
newText =
    StrElmnt text


newBold : String -> Elem
newBold =
    StrElmnt bold


newItalic : String -> Elem
newItalic =
    StrElmnt italic


newStrike : String -> Elem
newStrike =
    StrElmnt strike


newUnderline : String -> Elem
newUnderline =
    StrElmnt underline


newSub : String -> Elem
newSub =
    StrElmnt sub


newSuper : String -> Elem
newSuper =
    StrElmnt super


newHairline : Style -> Elem
newHairline style =
    StyElmnt hairline style


newScreen : El -> Elem
newScreen el =
    ElmntElmnt screen el


newNode : String -> El -> Elem
newNode str =
    StrElmntElmnt node str


newLink : String -> El -> Elem
newLink src el =
    StrElmntElmnt link src el


newNewtab : String -> El -> Elem
newNewtab src el =
    StrElmntElmnt newTab src el


newDownload : String -> El -> Elem
newDownload src el =
    StrElmntElmnt download src el


newWhen : Bool -> El -> Elem
newWhen bool elm =
    BoolElmntElmnt when bool elm


newWithin : List El -> El -> Elem
newWithin nearbys parent =
    ListElmntElmntElmnt within nearbys parent


newAbove : List El -> El -> Elem
newAbove nearbys parent =
    ListElmntElmntElmnt above nearbys parent


newBelow : List El -> El -> Elem
newBelow nearbys parent =
    ListElmntElmntElmnt below nearbys parent


newOnright : List El -> El -> Elem
newOnright nearbys parent =
    ListElmntElmntElmnt onRight nearbys parent


newOnleft : List El -> El -> Elem
newOnleft nearbys parent =
    ListElmntElmntElmnt onLeft nearbys parent


newSubheading : Style -> List Attr -> String -> Elem
newSubheading style attrs str =
    StyListAttrStrElmnt subheading style attrs str


newEl : Style -> List Attr -> El -> Elem
newEl style attrs child =
    StyListAttrElmntElmnt el style attrs child


newSection : Style -> List Attr -> El -> Elem
newSection style attrs child =
    StyListAttrElmntElmnt section style attrs child


newArticle : Style -> List Attr -> El -> Elem
newArticle style attrs child =
    StyListAttrElmntElmnt article style attrs child


newAside : Style -> List Attr -> El -> Elem
newAside style attrs child =
    StyListAttrElmntElmnt aside style attrs child


newButton : Style -> List Attr -> El -> Elem
newButton style attrs child =
    StyListAttrElmntElmnt button style attrs child


newH1 : Style -> List Attr -> El -> Elem
newH1 style attrs child =
    StyListAttrElmntElmnt h1 style attrs child


newH2 : Style -> List Attr -> El -> Elem
newH2 style attrs child =
    StyListAttrElmntElmnt h2 style attrs child


newH3 : Style -> List Attr -> El -> Elem
newH3 style attrs child =
    StyListAttrElmntElmnt h3 style attrs child


newH4 : Style -> List Attr -> El -> Elem
newH4 style attrs child =
    StyListAttrElmntElmnt h4 style attrs child


newH5 : Style -> List Attr -> El -> Elem
newH5 style attrs child =
    StyListAttrElmntElmnt h5 style attrs child


newH6 : Style -> List Attr -> El -> Elem
newH6 style attrs child =
    StyListAttrElmntElmnt h6 style attrs child


newFull : Style -> List Attr -> El -> Elem
newFull elem attrs child =
    StyListAttrElmntElmnt full elem attrs child


newSearch : Style -> List Attr -> El -> Elem
newSearch style attrs child =
    StyListAttrElmntElmnt search style attrs child


newHeader : Style -> List Attr -> El -> Elem
newHeader style attrs child =
    StyListAttrElmntElmnt header style attrs child


newFooter : Style -> List Attr -> El -> Elem
newFooter style attrs child =
    StyListAttrElmntElmnt footer style attrs child


newMaincontent : Style -> List Attr -> El -> Elem
newMaincontent style attrs child =
    StyListAttrElmntElmnt mainContent style attrs child


newModal : Style -> List Attr -> El -> Elem
newModal style attrs child =
    StyListAttrElmntElmnt modal style attrs child


newCircle : Float -> Style -> List Attr -> El -> Elem
newCircle radius style attrs child =
    FltStyListAttrElmntElmnt circle radius style attrs child


newTextlayout : Style -> List Attr -> List El -> Elem
newTextlayout style attrs children =
    StyListAttrListElmntElmnt textLayout style attrs children


newParagraph : Style -> List Attr -> List El -> Elem
newParagraph style attrs children =
    StyListAttrListElmntElmnt paragraph style attrs children


newRow : Style -> List Attr -> List El -> Elem
newRow style attrs children =
    StyListAttrListElmntElmnt row style attrs children


newColumn : Style -> List Attr -> List El -> Elem
newColumn style attrs children =
    StyListAttrListElmntElmnt column style attrs children


newWrappedrow : Style -> List Attr -> List El -> Elem
newWrappedrow style attrs children =
    StyListAttrListElmntElmnt wrappedRow style attrs children


newWrappedcolumn : Style -> List Attr -> List El -> Elem
newWrappedcolumn style attrs children =
    StyListAttrListElmntElmnt wrappedColumn style attrs children


newSidebar : Style -> List Attr -> List El -> Elem
newSidebar style attrs children =
    StyListAttrListElmntElmnt sidebar style attrs children


type Attr
    = Attr AttrFn
    | FltAttr FltAttrFn Float
    | LngAttr LngAttrFn Lngth
    | StrAttr StrAttrFn String
    | FltFltAttr FltFltAttrFn Float Float


{-| Case statement template:
-}
caseAttr attr =
    case attr of
        Attr f ->
            attr

        FltAttr f flt0 ->
            attr

        LngAttr f lng0 ->
            attr

        StrAttr f str0 ->
            attr

        FltFltAttr f flt0 flt1 ->
            attr


type alias AttrFn =
    Attribute Variation Msg


type alias FltAttrFn =
    Float -> Attribute Variation Msg


type alias LngAttrFn =
    Length -> Attribute Variation Msg


type alias StrAttrFn =
    String -> Attribute Variation Msg


type alias FltFltAttrFn =
    Float -> Float -> Attribute Variation Msg


newCenter : Attr
newCenter =
    Attr center


newVerticalcenter : Attr
newVerticalcenter =
    Attr verticalCenter


newVerticalspread : Attr
newVerticalspread =
    Attr verticalSpread


newSpread : Attr
newSpread =
    Attr spread


newAligntop : Attr
newAligntop =
    Attr alignTop


newAlignbottom : Attr
newAlignbottom =
    Attr alignBottom


newAlignleft : Attr
newAlignleft =
    Attr alignLeft


newAlignright : Attr
newAlignright =
    Attr alignRight


newHidden : Attr
newHidden =
    Attr hidden


newScrollbars : Attr
newScrollbars =
    Attr scrollbars


newYscrollbar : Attr
newYscrollbar =
    Attr yScrollbar


newXscrollbar : Attr
newXscrollbar =
    Attr xScrollbar


newClip : Attr
newClip =
    Attr clip


newClipx : Attr
newClipx =
    Attr clipX


newClipy : Attr
newClipy =
    Attr clipY


newMoveup : Float -> Attr
newMoveup y =
    FltAttr moveUp y


newMovedown : Float -> Attr
newMovedown y =
    FltAttr moveDown y


newMoveright : Float -> Attr
newMoveright x =
    FltAttr moveRight x


newMoveleft : Float -> Attr
newMoveleft x =
    FltAttr moveLeft x


newSpacing : Float -> Attr
newSpacing x =
    FltAttr spacing x


newPadding : Float -> Attr
newPadding x =
    FltAttr padding x


newPaddingleft : Float -> Attr
newPaddingleft x =
    FltAttr paddingLeft x


newPaddingright : Float -> Attr
newPaddingright x =
    FltAttr paddingRight x


newPaddingtop : Float -> Attr
newPaddingtop x =
    FltAttr paddingTop x


newPaddingbottom : Float -> Attr
newPaddingbottom x =
    FltAttr paddingBottom x


newWidth : Lngth -> Attr
newWidth =
    LngAttr width


newMinwidth : Lngth -> Attr
newMinwidth len =
    LngAttr minWidth len


newMaxwidth : Lngth -> Attr
newMaxwidth len =
    LngAttr maxWidth len


newMinheight : Lngth -> Attr
newMinheight len =
    LngAttr minHeight len


newMaxheight : Lngth -> Attr
newMaxheight len =
    LngAttr maxHeight len


newHeight : Lngth -> Attr
newHeight =
    LngAttr height


newClass : String -> Attr
newClass cls =
    StrAttr class cls


newId : String -> Attr
newId str =
    StrAttr id str


newSpacingxy : Float -> Float -> Attr
newSpacingxy =
    FltFltAttr spacingXY


newPaddingxy : Float -> Float -> Attr
newPaddingxy x y =
    FltFltAttr paddingXY x y


type Lngth
    = Lng LngFn
    | FltLng FltLngFn Float
    | IntLng IntLngFn Int


{-| Case statement template:
-}
caseLngth lngth =
    case lngth of
        Lng f ->
            lngth

        FltLng f flt0 ->
            lngth

        IntLng f int0 ->
            lngth


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
