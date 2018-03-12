module Subscriptions.Subscriptions exposing (subscriptions)

import Data.Storage
import Model.Model exposing (Model)
import Model.Types exposing (Msg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Data.Storage.onLoad
