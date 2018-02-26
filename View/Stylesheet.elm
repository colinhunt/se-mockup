module View.Stylesheet exposing (..)

import Color
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Border as Border
import Style.Background as Background
import Style.Shadow as Shadow


type Variation
    = Hover
    | Selected


type Style
    = None
    | Elmnt
    | Row
    | Column
    | Button
    | ThingPicker
    | NameButton
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
    , variation Hover [ Color.background <| Color.greyscale 0.1 ]
    , variation Selected
        [ Color.background paleBlue
        ]
    ]
        ++ styles


stylesheet : StyleSheet Style Variation
stylesheet =
    Style.styleSheet
        [ style None [] -- blank style
        , style Elmnt <|
            elemBase
                []
        , style Button []
        , style NameButton <|
            thingName
                ++ [ Color.background unsetColor ]
        , style Input <|
            thingName
                ++ [ Color.text Color.orange
                   , Color.background unsetColor
                   ]
        , style (ElementInfo EiMain)
            [ Color.background Color.lightGray
            ]
        , style (ElementInfo EiTitle) <|
            thingName
                ++ [ Font.size 25
                   , Font.bold
                   ]
        , style ThingPicker
            [ Color.background Color.grey ]
        , style (TreeView TvLabel)
            [ Color.background <| unsetColor
            , variation Selected [ Color.text Color.orange ]
            ]
        , style (TreeView TvNode)
            [ Border.left 1
            , Color.border Color.grey
            ]
        , style (CodeView CvMain)
            [ Color.background Color.lightGray ]
        , style (CodeView CvTextArea)
            [ Font.typeface [ Font.monospace ]
            , prop "white-space" "pre"
            ]
        ]
