module View.Stylesheet exposing (..)

import Color
import Style exposing (..)
import Style.Background as Background
import Style.Border as Border
import Style.Color as SColor
import Style.Filter as Filter
import Style.Font as Font
import Style.Shadow as Shadow


type Variation
    = Hover
    | Selected
    | SelectedEntry


type Style
    = None
    | Main
    | Element
    | Button
    | ButtonLabel
    | ThingPicker
    | NameButton
    | PickerButton
    | Input
    | InputNormal
    | Modal
    | ModalBackdrop
    | ElementInfo ElementInfoStyle
    | TreeView TreeViewStyle
    | CodeView CodeViewStyle
    | TopMenu TopMenuStyle


type ElementInfoStyle
    = EiMain
    | EiTitle
    | EiThingInfo
    | EiEntry


type TreeViewStyle
    = TvMain
    | TvNode
    | TvLabel


type CodeViewStyle
    = CvMain
    | CvTextArea


type TopMenuStyle
    = TmMain
    | TmTitleBar
    | TmTitle
    | TmStatus
    | TmButtonRow


paleBlue =
    Color.rgb 194 224 255


transparentCobalt =
    Color.rgba 71 82 93 0.8


unsetColor =
    Color.rgba 0 0 0 0


thingName =
    [ Font.typeface [ Font.monospace ] ]


elemBase styles =
    [ Shadow.inset
        { offset = ( 0, 0 )
        , size = 1
        , blur = 0
        , color = transparentCobalt
        }
    , variation Hover [ SColor.background <| Color.greyscale 0.1 ]
    , variation Selected
        [ SColor.background paleBlue
        ]
    ]
        ++ styles


font =
    Font.typeface [ Font.font "Trebuchet MS" ]


iconColor =
    Color.darkCharcoal


stylesheet : StyleSheet Style Variation
stylesheet =
    Style.styleSheet
        [ style None [] -- blank style
        , style Main
            [ font ]
        , style Element <|
            elemBase
                []
        , style Button
            [ SColor.background unsetColor
            , SColor.text Color.darkGrey
            , Border.rounded 2
            , hover
                [ SColor.text Color.black
                , SColor.background Color.gray
                ]
            , cursor "pointer"
            , pseudo "active" [ SColor.background Color.darkGray ]
            ]
        , style ButtonLabel
            []
        , style NameButton <|
            thingName
                ++ [ SColor.background unsetColor
                   , Border.rounded 2
                   , variation SelectedEntry
                        [ SColor.background Color.charcoal
                        , SColor.text Color.white
                        ]
                   , hover [ SColor.background Color.lightCharcoal ]
                   ]
        , style Input <|
            thingName
                ++ [ SColor.text Color.orange
                   , SColor.background unsetColor
                   ]
        , style Modal <|
            [ font
            , SColor.background Color.white
            , SColor.border Color.darkGray
            , Border.all 1
            , Shadow.deep
            ]
        , style ModalBackdrop
            [ SColor.background transparentCobalt ]
        , style InputNormal
            [ Border.all 1
            , SColor.border Color.gray
            ]
        , style (ElementInfo EiMain)
            [ SColor.border Color.grey
            , Border.right 1
            , Border.left 1
            ]
        , style (ElementInfo EiTitle) <|
            thingName
                ++ [ Font.size 25
                   , Font.bold
                   , SColor.border Color.grey
                   , Border.bottom 1
                   ]
        , style (ElementInfo EiThingInfo)
            [ SColor.border Color.lightGray
            , Border.top 1
            ]
        , style ThingPicker
            [ SColor.background Color.charcoal
            , Border.rounded 5
            , Filter.opacity 92
            ]
        , style PickerButton <|
            thingName
                ++ [ SColor.text Color.white
                   , SColor.background unsetColor
                   , hover [ SColor.background Color.black ]
                   , cursor "pointer"
                   ]
        , style (TreeView TvMain)
            []
        , style (TreeView TvLabel)
            [ SColor.background <| unsetColor
            , variation Selected [ SColor.text Color.orange ]
            ]
        , style (TreeView TvNode)
            [ Border.left 1
            , SColor.border Color.grey
            ]
        , style (CodeView CvMain)
            [ Border.all 1
            , SColor.border Color.grey
            ]
        , style (CodeView CvTextArea)
            [ Font.typeface [ Font.monospace ]
            , prop "white-space" "pre"
            ]
        , style (TopMenu TmMain)
            [ Border.all 1
            , SColor.border Color.grey
            ]
        ]
