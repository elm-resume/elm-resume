module Main exposing (main)

import Html exposing (..)
import Model exposing (..)
import Action exposing (..)

main : Program Never Model Action
main =
  Html.program
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


view : Model -> Html Action
view model =
  div []
    [ text "New Html Program"
    ]


subscriptions : Model -> Sub Action
subscriptions model =
  Sub.none


init : (Model, Cmd Action)
init =
  (initModel, Cmd.none)
