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
    | ElementInfo
    | ElName
    | ThingPicker
    | TreeLabel
    | TreeNode


paleBlue =
    Color.rgb 194 224 255


transparentCobalt =
    Color.rgba 71 82 93 0.8


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
        , style Row <|
            elemBase
                [ Color.border Color.blue
                ]
        , style Column <|
            elemBase
                [ Color.border Color.orange
                ]
        , style Button []
        , style ElementInfo
            [ Color.background Color.lightGray
            ]
        , style ElName
            [ Font.size 25
            , Font.bold
            , Font.typeface [ Font.monospace ]
            ]
        , style ThingPicker
            [ Color.background Color.grey ]
        , style TreeLabel
            [ Color.background <| Color.rgba 0 0 0 0
            , variation Selected [ Color.text Color.orange ]
            ]
        , style TreeNode
            [ Border.left 1
            , Color.border Color.grey
            ]
        ]
