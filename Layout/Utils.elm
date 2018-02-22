module Layout.Utils exposing (..)

import Json.Decode as Json
import Element exposing (..)
import Element.Events exposing (onClick, onWithOptions)
import Element.Attributes exposing (..)
import Element.Input as Input
import Layout.Element exposing (Elid)
import View.Stylesheet as Sty


type Picker
    = AddAttribute
    | ReplaceLength String
    | ReplaceChild Elid
    | ReplaceElement
    | AddChild
    | None


onClickNoProp msg =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } <|
        Json.succeed msg


textInput : (String -> msg) -> String -> Input.Label Sty.Style var msg -> String -> Element Sty.Style var msg
textInput onChange str label key =
    Input.text Sty.None
        []
        { onChange = onChange
        , value = str
        , label = label
        , options = [ Input.textKey key ]
        }


viewInfoStr : (String -> msg) -> String -> String -> Element Sty.Style var msg
viewInfoStr onChange str =
    textInput onChange str (Input.labelAbove <| text "Text:")


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
    String
    -> String
    -> msg
    -> Bool
    -> List (Element Sty.Style var msg)
    -> List (Element Sty.Style var msg)
    -> Element Sty.Style var msg
thingInfo title newThingBttnTxt onNewThingBttn showNewThings things newThings =
    column Sty.None [] <|
        [ text title ]
            ++ things
            ++ [ thingButton onNewThingBttn newThingBttnTxt showNewThings newThings ]


newThingBttn : ({ r | name : String } -> msg) -> { r | name : String } -> Element Sty.Style var msg
newThingBttn onThing newThing =
    el Sty.None [ onClick <| onThing newThing ] <| text newThing.name


thingButton onThingBttn bttnTxt showNewThings newThings =
    (el Sty.None [] <|
        button Sty.None
            [ onClickNoProp onThingBttn ]
        <|
            text bttnTxt
    )
        |> below [ when showNewThings <| wrappedRow Sty.ThingPicker [ moveRight 10, spacing 5 ] newThings ]