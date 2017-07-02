module View exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, h2, li, span, text, ul)
import Html.Attributes exposing (class)
import Model exposing (Model, Msg, Probe)


view : Model -> Html Msg
view model =
    let
        errors =
            case List.length model.errors of
                0 ->
                    []

                _ ->
                    div [ class "errors" ] (List.map text model.errors) :: []

        groups =
            case List.length model.probes of
                0 ->
                    []

                _ ->
                    ul [ class "groups" ] (groupProbes model.probes |> Dict.toList |> List.map (\group -> viewGroup (Tuple.second group) (Tuple.first group))) :: []
    in
    div [] <| errors ++ groups


groupProbes : List Probe -> Dict String (List Probe)
groupProbes probes =
    let
        update probe all =
            case all of
                Just all ->
                    Just ([ probe ] ++ all)

                Nothing ->
                    Nothing

        reduce probe acc =
            if Dict.member probe.group acc then
                Dict.update probe.group (update probe) acc
            else
                Dict.insert probe.group [ probe ] acc
    in
    List.foldr reduce Dict.empty probes


viewGroup : List Probe -> String -> Html Msg
viewGroup probes name =
    let
        childs =
            if not (name == "") then
                [ h2 [ class "group-name" ] [ text name ] ]
            else
                []

        appendProbes elements =
            elements ++ [ ul [ class "probes" ] (List.map probe probes) ]

        status status =
            case status of
                Just status ->
                    case status of
                        Ok value ->
                            if value >= 200 && value < 300 then
                                "Ok"
                            else
                                "Fail"

                        Err message ->
                            "Fail"

                Nothing ->
                    "..."

        badgeClassName status =
            case status of
                Just status ->
                    case status of
                        Ok value ->
                            if value >= 200 && value < 300 then
                                "badge ok"
                            else
                                "badge error"

                        Err message ->
                            "badge error"

                Nothing ->
                    "badge pending"

        probe probe =
            li [ class "probe" ]
                [ div [ class "probe-name" ] [ text probe.name ]
                , div [ class "probe-status" ]
                    [ div [ class <| badgeClassName probe.status ] [ text <| status probe.status ]
                    ]
                ]
    in
    li [ class "group" ] (appendProbes childs)
