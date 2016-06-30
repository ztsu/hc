import Html.App
import Html exposing (Html, div, text, span, ul, li, h1, h2)
import Html.Attributes exposing (class)
import Dict exposing (Dict)
import Http
import Task
import Json.Decode exposing ((:=))

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
  case flags.manifestUrl of
    Just url -> ( Model flags.manifestUrl [] [], getProbes url )
    Nothing -> ( Model flags.manifestUrl [] ["Не указан url до манифест-файла"], Cmd.none )


getProbes url =
  Task.perform ReceiveProbesError ReceiveProbes ( Http.get decodeProbes url )



decodeProbes =
  Json.Decode.list <| Json.Decode.object4 Probe
    ( "name" := Json.Decode.string )
    ( "group" := Json.Decode.string )
    ( "url" := Json.Decode.string )
    ( Json.Decode.maybe <| Json.Decode.fail "" )

touchProbe maybeUrl =
  let
    req url =
    { verb = "GET"
    , headers = []
    , url = url
    , body = Http.empty
    }
  in
    case maybeUrl of
      Just probe ->
        Http.send Http.defaultSettings (req probe.url)
          |> Task.map (\r -> r.status)
          |> Task.perform (TouchProbeError probe.url) (TouchProbeResult probe.url)

      Nothing -> Cmd.none

touchNextProbe probes skipUrl =
  let
    f probe =
      case probe.status of
        Just status -> False
        Nothing -> not (probe.url == skipUrl)
  in
    List.filter f probes |> List.head |> touchProbe




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ReceiveProbes probes ->
      ({ model | probes = probes }, touchNextProbe probes "")

    ReceiveProbesError error ->
      ({ model | errors = "Ошибка при получении манифест-файла" :: model.errors }, Cmd.none )

    TouchProbeResult url status ->
      ({ model | probes = model.probes |> List.map (\probe -> if probe.url == url then { probe | status = Just (Ok status) } else probe) }, touchNextProbe model.probes url)

    TouchProbeError url _ ->
      ({ model | probes = model.probes |> List.map (\probe -> if probe.url == url then { probe | status = Just (Err "Error") } else probe) }, Cmd.none )


subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  let
    header =
      h1 [] [ text "billing.ngs.ru" ]
    errors =
      if List.length model.errors == 0
        then []
      else
        div [ class "errors" ] ( List.map text model.errors) :: []

    groups =
      ul [ class "groups" ] (groupProbes model.probes |> Dict.toList |> List.map (\group -> viewGroup (snd group) (fst group))) :: []

  in
    div [] <| header :: errors ++ groups


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


main : Program Flags
main =
  Html.App.programWithFlags { init = init, update = update, subscriptions = subscriptions, view = view}
