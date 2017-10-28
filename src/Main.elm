module Main exposing (main)

import Html exposing (program)
import Model exposing (..)
import Action exposing (..)
import View exposing (view)

main : Program Never Model Action
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
  }

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    Do ->
      (model, Cmd.none)


subscriptions : Model -> Sub Action
subscriptions model =
  Sub.none


init : (Model, Cmd Action)
init =
  (initModel, Cmd.none)
