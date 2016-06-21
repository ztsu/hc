import Html.App
import Html exposing (Html, div, text, span, ul, li)
import Dict exposing (Dict)
import Http
import Task
import Json.Decode exposing ((:=))

type alias Probe =
  { name : String
  , group: String
  , url: String
  , status: Maybe Int
  }


type alias Model =
  { manifestUrl : String
  , probes : List Probe
  , errors : List String
  }


type Msg
  = ReceiveProbes (List Probe)
  | ReceiveProbesError Http.Error


type alias Flags = { manifestUrl : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
  ( Model flags.manifestUrl [] []
  , getProbes flags.manifestUrl
  )


getProbes url =
  Task.perform ReceiveProbesError ReceiveProbes ( Http.get decodeProbes url )


decodeProbes =
  Json.Decode.list <| Json.Decode.object4 Probe
    ( "name" := Json.Decode.string )
    ( "group" := Json.Decode.string )
    ( "url" := Json.Decode.string )
    ( Json.Decode.maybe ("status" := Json.Decode.int) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ReceiveProbes probes ->
      ({ model | probes = probes }, Cmd.none )

    ReceiveProbesError error ->
      ({ model | errors = "Ошибка при получении манифест-файла" :: model.errors }, Cmd.none )


subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ div [] ( List.map text model.errors )
    , div [] [ text model.manifestUrl ]
    , ul [] ( groupProbes model.probes |> Dict.toList |> List.map (\group -> viewGroup (snd group) (fst group)) )
    ]


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

    status status =
      case status of
        Just status -> toString status
        Nothing -> "..."

    probe probe =
      li []
      [ span [] [ text probe.name ]
      , span [] [ text <| status probe.status ]
      ]
  in
    li [] (appendProbes childs)


main : Program Flags
main =
  Html.App.programWithFlags { init = init, update = update, subscriptions = subscriptions, view = view}
