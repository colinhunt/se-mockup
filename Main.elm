module Main exposing (..)

import Html
import Update.Update exposing (update)
import Model.Model exposing (Model, Msg, initModel)
import View.View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { init = initModel ! []
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
