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


elemBase styles =
    [ Border.all 1
    , Border.rounded 3
    , variation Hover [ Color.background <| Color.greyscale 0.1 ]
    , variation Selected
        [ Shadow.inset
            { offset = ( 0, 0 )
            , size = 0
            , blur = 5
            , color = Color.lightBlue
            }
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
        ]
