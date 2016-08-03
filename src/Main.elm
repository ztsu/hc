import Html.App
import Html exposing (Html, div, text, span, ul, li, h1, h2)
import Html.Attributes exposing (class)
import Dict exposing (Dict)
import Http
import Task
import Json.Decode exposing ((:=))


-- Model


type alias Probe =
  { name : String
  , group: String
  , url: String
  , status: Maybe (Result String Int)
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
  | TouchProbeError String Http.RawError


type alias Flags = { manifestUrl : Maybe String }


init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    decodeProbes =
      Json.Decode.list <| Json.Decode.object4 Probe
        ( "name" := Json.Decode.string )
        ( "group" := Json.Decode.string )
        ( "url" := Json.Decode.string )
        ( Json.Decode.maybe <| Json.Decode.fail "" )

    getProbes url =
      Task.perform ReceiveProbesError ReceiveProbes ( Http.get decodeProbes url )
  in
    case flags.manifestUrl of
      Just url ->
        ( Model flags.manifestUrl [] [], getProbes url )

      Nothing ->
        ( Model flags.manifestUrl [] ["Не указан url до манифест-файла"], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    findProbeAndUpdateStatus url status =
      model.probes |> List.map ( \probe -> if probe.url == url then { probe | status = Just status } else probe )

    hasNothingStatus : Probe -> Bool
    hasNothingStatus probe =
      case probe.status of
        Just status ->
          False

        Nothing ->
          True

    makeRequest : String -> { body : Http.Body, headers : List a, url : String, verb : String }
    makeRequest url =
      { verb = "GET"
      , headers = []
      , url = url
      , body = Http.empty
      }

    touchProbe : Maybe Probe -> Cmd Msg
    touchProbe maybeUrl =
      case maybeUrl of
        Just probe ->
          Http.send Http.defaultSettings (makeRequest probe.url)
            |> Task.map (\r -> r.status)
            |> Task.perform (TouchProbeError probe.url) (TouchProbeResult probe.url)

        Nothing -> Cmd.none

    touchNextProbe : List Probe -> Cmd Msg
    touchNextProbe probes =
      touchProbe <| List.head <| List.filter hasNothingStatus <| probes

  in
    case msg of
      ReceiveProbes probes ->
        ({ model | probes = probes }, touchNextProbe probes)

      ReceiveProbesError error ->
        ({ model | errors = "Ошибка при получении манифест-файла" :: model.errors }, Cmd.none)

      TouchProbeResult url status ->
        ({ model | probes = findProbeAndUpdateStatus url (Ok status)}, findProbeAndUpdateStatus url (Ok status) |> touchNextProbe)

      TouchProbeError url _ ->
        ({ model | probes = findProbeAndUpdateStatus url (Err "Error")}, findProbeAndUpdateStatus url (Err "Error") |> touchNextProbe)


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
          div [ class "errors" ] ( List.map text model.errors) :: []

    groups =
      case List.length model.probes of
        0 -> []

        _ ->
          ul [ class "groups" ] ( groupProbes model.probes |> Dict.toList |> List.map (\group -> viewGroup (snd group) (fst group)) ) :: []

  in
    div [] <| errors ++ header ++ groups


groupProbes : List Probe -> Dict String (List Probe)
groupProbes probes =
  let
    update probe all =
      case all of
        Just all ->
          Just ([probe] ++ all)

        Nothing ->
          Nothing

    reduce probe acc =
      if Dict.member probe.group acc then
        Dict.update probe.group (update probe) acc
      else
        Dict.insert probe.group [probe] acc

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
            Ok value -> toString value
            Err message -> message
        Nothing -> "..."

    badgeClassName status =
      case status of
        Just status ->
          case status of
            Ok value -> "badge ok"
            Err message -> "badge error"
        Nothing -> "badge pending"

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


main : Program Flags
main =
  Html.App.programWithFlags
    { init = init
    , update = update
    , subscriptions = \m -> Sub.none
    , view = view
    }
