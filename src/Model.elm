module Model exposing (..)

import Http
import Json.Decode exposing (field)


type alias Model =
    { manifestUrl : Maybe String
    , probes : List Probe
    , errors : List String
    }


type alias Probe =
    { name : String
    , group : String
    , url : String
    , status : Maybe (Result String Int)
    }


type Msg
    = ReceiveProbes (List Probe)
    | ReceiveProbesError Http.Error
    | TouchProbeResult String Int
    | TouchProbeError String Http.Error


type alias Flags =
    { manifestUrl : Maybe String }


init : { manifestUrl : Maybe String } -> ( Model, Cmd Msg )
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
            ( Model flags.manifestUrl [] [ "A path to file manifest is not specified" ], Cmd.none )


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
            ( { model | errors = "Couldn't get a file manifest" :: model.errors }, Cmd.none )

        TouchProbeResult url status ->
            ( { model | probes = findProbeAndUpdateStatus url (Ok status) }, findProbeAndUpdateStatus url (Ok status) |> touchNextProbe )

        TouchProbeError url _ ->
            ( { model | probes = findProbeAndUpdateStatus url (Err "Error") }, findProbeAndUpdateStatus url (Err "Error") |> touchNextProbe )
