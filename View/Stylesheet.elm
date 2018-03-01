module View.Stylesheet exposing (..)

import Color
import Style exposing (..)
import Style.Color as SColor
import Style.Font as Font
import Style.Border as Border
import Style.Background as Background
import Style.Shadow as Shadow
import Style.Filter as Filter


type Variation
    = Hover
    | Selected


type Style
    = None
    | Main
    | Elmnt
    | Button
    | ThingPicker
    | NameButton
    | PickerButton
    | Input
    | ElementInfo ElementInfoStyle
    | TreeView TreeViewStyle
    | CodeView CodeViewStyle


type ElementInfoStyle
    = EiMain
    | EiTitle
    | EiThingInfo


type TreeViewStyle
    = TvMain
    | TvNode
    | TvLabel


type CodeViewStyle
    = CvMain
    | CvTextArea


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


stylesheet : StyleSheet Style Variation
stylesheet =
    Style.styleSheet
        [ style None [] -- blank style
        , style Main
            [ Font.typeface [ Font.sansSerif ] ]
        , style Elmnt <|
            elemBase
                []
        , style Button
            [ SColor.background unsetColor
            , SColor.text Color.darkGrey
            ]
        , style NameButton <|
            thingName
                ++ [ SColor.background unsetColor ]
        , style Input <|
            thingName
                ++ [ SColor.text Color.orange
                   , SColor.background unsetColor
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
        ]
