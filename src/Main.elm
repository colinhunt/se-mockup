module Main exposing (..)

import Html
import Model.Model exposing (Model, init)
import Model.Types exposing (Msg)
import Subscriptions.Subscriptions exposing (subscriptions)
import Update.Update exposing (update)
import View.View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
