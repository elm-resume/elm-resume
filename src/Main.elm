module Main exposing (main)

import Html
import Http
import Model exposing (..)
import Action exposing (..)
import View exposing (view)
import Config exposing (Config)
import RemoteData

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
  case action of
    RequestedData res ->
      ({model | resume = res }, Cmd.none)


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
