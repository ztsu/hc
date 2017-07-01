module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, h1, h2, li, span, text, ul)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (field)
import Task
import Tuple


-- Model


type alias Probe =
    { name : String
    , group : String
    , url : String
    , status : Maybe (Result String Int)
    }


type alias Model =
    { manifestUrl : Maybe String
    , probes : List Probe
    , errors : List String
    }


type Msg
    = ReceiveProbes (List Probe)
    | ReceiveProbesError Http.Error
    | TouchProbeResult String Int
    | TouchProbeError String Http.Error


type alias Flags =
    { manifestUrl : Maybe String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        decodeProbes =
            Json.Decode.list <|
                Json.Decode.map4 Probe
                    (field "name" Json.Decode.string)
                    (field "group" Json.Decode.string)
                    (field "url" Json.Decode.string)
                    (Json.Decode.maybe <| Json.Decode.fail "")

        handleResult result =
            case result of
                Ok probes ->
                    ReceiveProbes probes

                Err error ->
                    ReceiveProbesError error

        getProbes url =
            Http.send handleResult <| Http.get url decodeProbes
    in
    case flags.manifestUrl of
        Just url ->
            ( Model flags.manifestUrl [] [], getProbes url )

        Nothing ->
            ( Model flags.manifestUrl [] [ "Не указан url до манифест-файла" ], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        findProbeAndUpdateStatus url status =
            model.probes
                |> List.map
                    (\probe ->
                        if probe.url == url then
                            { probe | status = Just status }
                        else
                            probe
                    )

        hasNothingStatus : Probe -> Bool
        hasNothingStatus probe =
            case probe.status of
                Just status ->
                    False

                Nothing ->
                    True

        makeRequest : String -> Http.Request Int
        makeRequest url =
            Http.request
                { method = "GET"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectStringResponse (\res -> Result.Ok res.status.code)
                , timeout = Nothing
                , withCredentials = False
                }

        handleResult url result =
            case result of
                Ok status ->
                    TouchProbeResult url status

                Err error ->
                    TouchProbeError url error

        touchProbe : Maybe Probe -> Cmd Msg
        touchProbe maybeUrl =
            case maybeUrl of
                Just probe ->
                    Http.send (handleResult probe.url) (makeRequest probe.url)

                Nothing ->
                    Cmd.none

        touchNextProbe : List Probe -> Cmd Msg
        touchNextProbe probes =
            touchProbe <| List.head <| List.filter hasNothingStatus <| probes
    in
    case msg of
        ReceiveProbes probes ->
            ( { model | probes = probes }, touchNextProbe probes )

        ReceiveProbesError error ->
            ( { model | errors = "Ошибка при получении манифест-файла" :: model.errors }, Cmd.none )

        TouchProbeResult url status ->
            ( { model | probes = findProbeAndUpdateStatus url (Ok status) }, findProbeAndUpdateStatus url (Ok status) |> touchNextProbe )

        TouchProbeError url _ ->
            ( { model | probes = findProbeAndUpdateStatus url (Err "Error") }, findProbeAndUpdateStatus url (Err "Error") |> touchNextProbe )



-- View


view : Model -> Html Msg
view model =
    let
        header =
            case List.length model.probes of
                0 ->
                    []

                _ ->
                    h1 [] [ text "Report" ] :: []

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
    div [] <| errors ++ header ++ groups


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



--


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , view = view
        }
