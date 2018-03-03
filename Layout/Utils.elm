module Layout.Utils exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick, onWithOptions)
import Element.Input as Input
import Json.Decode as Json
import Layout.Element exposing (Elid)
import Utils as U
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


type Direction
    = Up
    | Down


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
    column (Sty.ElementInfo Sty.EiThingInfo) [ padding 5 ] <|
        [ text a.title
        , column Sty.None [ paddingLeft 10 ] <|
            a.things
                ++ [ thingButton
                        { style = Sty.Button
                        , onThingBttn = a.onNewThingBttn
                        , showNewThings = a.showNewThings
                        , newThings = a.newThings
                        , bttnTxt = a.newThingBttnTxt
                        , pickerAlignment = alignLeft
                        }
                   ]
        ]


newThingBttn : ({ r | name : String } -> msg) -> { r | name : String } -> Element Sty.Style var msg
newThingBttn onThing newThing =
    el Sty.PickerButton
        [ width (px 140)
        , padding 2.5
        , onClick <| onThing newThing
        ]
    <|
        text newThing.name


thingButton :
    { style : Sty.Style
    , onThingBttn : msg
    , showNewThings : Bool
    , newThings : List (Element Sty.Style var msg)
    , bttnTxt : String
    , pickerAlignment : Attribute var msg
    }
    -> Element Sty.Style var msg
thingButton props =
    (el Sty.None [] <|
        button props.style
            [ U.onClickNoProp props.onThingBttn ]
        <|
            text props.bttnTxt
    )
        |> below
            [ when props.showNewThings <|
                el Sty.None [ props.pickerAlignment ] <|
                    column Sty.ThingPicker
                        [ width (px 140)
                        , maxHeight (px 200)
                        , yScrollbar
                        , alignLeft
                        ]
                        props.newThings
            ]


thingControl :
    { title : Element Sty.Style var msg
    , things :
        { before : List (Element Sty.Style var msg)
        , selected : Maybe { menu : Element Sty.Style var msg, thing : Element Sty.Style var msg }
        , after : List (Element Sty.Style var msg)
        }
    , newThingButton : Element Sty.Style var msg
    }
    -> Element Sty.Style var msg
thingControl props =
    let
        ( menu, selected ) =
            case props.things.selected of
                Just { menu, thing } ->
                    ( menu, thing )

                Nothing ->
                    ( empty, empty )
    in
    within
        [ el Sty.None [ alignRight, moveLeft 5 ] menu
        ]
    <|
        column (Sty.ElementInfo Sty.EiThingInfo) [ padding 5 ] <|
            [ props.title
            , column Sty.None [ paddingLeft 10 ] <|
                props.things.before
                    ++ [ selected ]
                    ++ props.things.after
                    ++ [ props.newThingButton ]
            ]
