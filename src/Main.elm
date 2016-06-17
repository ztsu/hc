import Html.App
import Html exposing (div, text, span, ul, li)
import Dict exposing (Dict)

type alias Probe =
    { name : String
    , group: String
    , url: String
    }


probes =
    [ Probe "test1" "group1" "http://localhost/1"
    , Probe "test2" "group1" "http://localhost/2"
    , Probe "test3" "" "http://localhost/3"
    , Probe "test4" "group2" "http://localhost/4"
    ]


type alias Model = {
    probes: List Probe
}


init =
    ( Model probes
    , Cmd.none
    )


update model =
    model

subscriptions model =
    Sub.none


view model =
    ul [] ( groupProbes probes |> Dict.toList |> List.map (\group -> viewGroup (snd group) (fst group) ))


groupProbes probes =
    let
        update probe all =
            case all of
                Just all -> Just ([probe] ++ all)
                Nothing -> Nothing

        reduce probe acc =
            if Dict.member probe.group acc then
                Dict.update probe.group (update probe) acc
            else
                Dict.insert probe.group [probe] acc
    in
        List.foldr reduce Dict.empty probes


viewGroup probes name =
    let
        childs =
            [ div [] [ text name ]
            ]
        appendProbes elements =
            elements ++ [ ul [] (List.map probe probes) ]
        probe probe =
            li [] [ text probe.name ]
    in
        li [] (appendProbes childs)


main = Html.App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
