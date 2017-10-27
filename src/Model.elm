module Model exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

type alias Model =
  {
  }

initModel : Model
initModel = {}

type alias Resume =
  { name : String
  , address : String
  , contacts : List ContactWithPriority
  }

resumeDecoder : Decoder Resume
resumeDecoder =
  decode Resume
    |> required "name" string
    |> required "address" string
    |> optional "contacts" (list contactWithPriorityDecoder) []

type Priority
  = Primary
  | Secondary

priorityDecoder : Decoder Priority
priorityDecoder =
  let
    match s = case s of
      "primary" -> succeed Primary
      "secondary" -> succeed Secondary
      other -> fail <| "invalid priority: " ++ other
  in
    andThen match string

type Contact
  = Twitter String
  | Github String
  | Skype String

contactDecoder : Decoder Contact
contactDecoder =
  let
    matchValue s c d =
      let
        matchType t =
          case s == t of
            True -> decode c |> required "value" d
            False -> fail <| "No match for " ++ t
      in
        andThen matchType <| field "type" string
  in
    oneOf
      [ matchValue "github"  Github  string
      , matchValue "twitter" Twitter string
      , matchValue "skype"   Skype   string
      ]

type alias ContactWithPriority =
  { contact : Contact
  , priority : Priority
  }

contactWithPriorityDecoder : Decoder ContactWithPriority
contactWithPriorityDecoder =
  andThen (\c ->
    decode (ContactWithPriority c) |> optional "prio" priorityDecoder Primary
  ) contactDecoder

type UDate
  = Year Int
  | Month Int Int
  | Day Int Int Int
  | Unknown
  | Present

uDateParse : String -> Result String UDate
uDateParse s = Err "meh"

resultToDecoder : Result String a -> Decoder a
resultToDecoder r = case r of
  Err e -> fail e
  Ok v -> succeed v

uDateDecoder : Decoder UDate
uDateDecoder =
  andThen (resultToDecoder << uDateParse) string

-- dateOfBirth: "1972-05-07"
-- contacts:
--   - type: skype
--     value: francoponticelli
--     prio: primary
