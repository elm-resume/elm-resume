module Main exposing (main)

import Html
import Http
import Model exposing (..)
import Resume exposing (..)
import Action exposing (..)
import View exposing (view)
import Config exposing (Config)
import RemoteData
import Set

main : Program Config Model Action
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
  }

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case (action, model.resume) of
    (RequestedData res, _) ->
      ({ model | resume = RemoteData.map resumeToResumeState res }, Cmd.none)
    (ToggleOptionalSocialMedia, RemoteData.Success resume) ->
      let
        osm = resume.optionalSocialMedia
      in
        ({ model | resume = RemoteData.Success { resume | optionalSocialMedia = { osm | visible = not osm.visible } } }, Cmd.none)
    (ToggleItem id, RemoteData.Success resume) ->
      let
        contains = Set.member id resume.visible
        visible = (if contains then Set.remove else Set.insert) id resume.visible
      in
        ({ model | resume = RemoteData.Success { resume | visible = visible } }, Cmd.none)
    (_, _) ->
      (model, Cmd.none)


subscriptions : Model -> Sub Action
subscriptions model =
  Sub.none

getData : String -> Cmd Action
getData base_url =
  let
    url = base_url ++ "data.json"
  in
    Http.get url resumeDecoder
      |> RemoteData.sendRequest
      |> Cmd.map RequestedData

init : Config -> (Model, Cmd Action)
init cfg =
  initModel cfg ! [
    getData cfg.api_base_url
  ]
