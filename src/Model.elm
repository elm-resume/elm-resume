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
  , contact : Contact
  , socialMedia : List SocialMediaWithPriority
  , sections : List Section
  }

type alias Contact =
  {

  }

contactDecoder : Decoder Contact
contactDecoder = fail "not implemented"

-- name
-- contact
-- socialMedia
-- sections

resumeDecoder : Decoder Resume
resumeDecoder =
  decode Resume
    |> required "name" string
    |> required "contact" contactDecoder
    |> optional "socialMedia" (list socialMediaWithPriorityDecoder) []
    |> optional "sections" (list sectionDecoder) []

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

type SocialMedia
  = Twitter String
  | Github String
  | Skype String
  | LinkedIn String
  | StackOverflow String

socialMediaDecoder : Decoder SocialMedia
socialMediaDecoder =
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
      [ matchValue "github"        Github        string
      , matchValue "twitter"       Twitter       string
      , matchValue "skype"         Skype         string
      , matchValue "linkedin"      LinkedIn      string
      , matchValue "stackoverflow" StackOverflow string
      ]

type alias SocialMediaWithPriority =
  { handle : SocialMedia
  , priority : Priority
  }

socialMediaWithPriorityDecoder : Decoder SocialMediaWithPriority
socialMediaWithPriorityDecoder =
  andThen (\c ->
    decode (SocialMediaWithPriority c) |> optional "prio" priorityDecoder Primary
  ) socialMediaDecoder

type alias Section =
  { title : String

  }

sectionDecoder : Decoder Section
sectionDecoder = fail "not implemented"

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
