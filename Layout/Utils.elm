module Layout.Utils exposing (..)

import Json.Decode as Json
import Element exposing (..)
import Element.Events exposing (onClick, onWithOptions)
import Element.Attributes exposing (..)
import Element.Input as Input
import Utils as U
import Layout.Element exposing (Elid)
import View.Stylesheet as Sty


type Picker
    = ReplaceElement
    | AddChild
    | ReplaceChild Elid
    | DeleteChild Elid
    | AddAttribute
    | ReplaceAttribute String
    | ReplaceLength String
    | None


textInput : (String -> msg) -> String -> Input.Label Sty.Style var msg -> String -> Element Sty.Style var msg
textInput onChange str label key =
    Input.text Sty.Input
        []
        { onChange = onChange
        , value = str
        , label = label
        , options = [ Input.textKey key ]
        }


viewInfoStr : (String -> msg) -> String -> String -> Element Sty.Style var msg
viewInfoStr onChange str =
    textInput onChange str (Input.hiddenLabel "string")


viewInfoFlt : (Float -> msg) -> Float -> String -> Element Sty.Style var msg
viewInfoFlt onChange flt =
    textInput (String.toFloat >> Result.withDefault flt >> onChange) (toString flt) (Input.hiddenLabel "float")


viewInfoInt : (Int -> msg) -> Int -> String -> Element Sty.Style var msg
viewInfoInt onChange int =
    textInput (String.toInt >> Result.withDefault int >> onChange) (toString int) (Input.hiddenLabel "int")


viewInfoBool : (Bool -> msg) -> Bool -> String -> Element Sty.Style var msg
viewInfoBool onChange bool key =
    Input.checkbox Sty.None
        []
        { onChange = onChange
        , checked = bool
        , label = empty
        , options = [ Input.textKey key ]
        }


thingInfo :
    { title : String
    , newThingBttnTxt : String
    , onNewThingBttn : msg
    , showNewThings : Bool
    , things : List (Element Sty.Style var msg)
    , newThings : List (Element Sty.Style var msg)
    }
    -> Element Sty.Style var msg
thingInfo a =
    column (Sty.ElementInfo Sty.EiThingInfo) [] <|
        [ text a.title
        , column Sty.None [ paddingLeft 10 ] <|
            a.things
                ++ [ thingButton
                        { style = Sty.None
                        , onThingBttn = a.onNewThingBttn
                        , showNewThings = a.showNewThings
                        , newThings = a.newThings
                        , bttnTxt = a.newThingBttnTxt
                        }
                   ]
        ]


newThingBttn : ({ r | name : String } -> msg) -> { r | name : String } -> Element Sty.Style var msg
newThingBttn onThing newThing =
    el Sty.NameButton [ onClick <| onThing newThing ] <| text newThing.name


thingButton :
    { style : Sty.Style
    , onThingBttn : msg
    , showNewThings : Bool
    , newThings : List (Element Sty.Style var msg)
    , bttnTxt : String
    }
    -> Element Sty.Style var msg
thingButton a =
    (el Sty.None [] <|
        button a.style
            [ U.onClickNoProp a.onThingBttn ]
        <|
            text a.bttnTxt
    )
        |> below [ when a.showNewThings <| wrappedRow Sty.ThingPicker [ moveRight 10, spacing 5, width (px 200) ] a.newThings ]
