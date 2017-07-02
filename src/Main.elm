module Main exposing (..)

import Html
import Model exposing (Model, Msg, init, update)
import View exposing (view)


main : Program { manifestUrl : Maybe String } Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , view = view
        }
