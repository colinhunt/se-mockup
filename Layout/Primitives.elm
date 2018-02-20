module Layout.Primitives exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import View.Stylesheet as Sty


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
