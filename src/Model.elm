module Model exposing (..)

import RemoteData exposing (RemoteData(..))
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

type alias Model =
  { resume : RemoteData String Resume
  }

initModel : Model
initModel =
  { resume = NotAsked
  }

type alias Resume =
  { name : String
  , contact : Contact
  , socialMedia : List SocialMediaWithPriority
  , sections : List Section
  }

type alias Contact =
  { address: String
  , email: String
  , phone : String
  }

contactDecoder : Decoder Contact
contactDecoder =
  decode Contact
    |> required "address" string
    |> required "email" string
    |> required "phone" string

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

type alias SkillItem =
  { title : String
  , body : String
  }

skillItemDecoder : Decoder SkillItem
skillItemDecoder =
  decode SkillItem
    |> required "title" string
    |> required "body" string

type alias ExperienceItem =
  { title : String
  , body : String
  , begin : UDate
  , end : Maybe UDate
  }

experienceItemDecoder : Decoder ExperienceItem
experienceItemDecoder =
  decode ExperienceItem
    |> required "title" string
    |> required "body" string
    |> required "begin" uDateDecoder
    |> optional "end" (oneOf
      [ null Nothing
      , (custom uDateDecoder <| decode Just)
      ]) Nothing

type SectionBody
  = Text String
  | Skills (List SkillItem)
  | Experience (List ExperienceItem)

sectionBodyDecoder : Decoder SectionBody
sectionBodyDecoder =
  let
    textDecoder = decode Text |> required "content" string
    skillsDecoder = decode Skills |> required "skills" (list skillItemDecoder)
    experienceDecoder = decode Experience |> required "experience" (list experienceItemDecoder)
  in
    oneOf
      [ textDecoder
      , skillsDecoder
      , experienceDecoder
      ]

type alias Section =
  { title : String
  , body : SectionBody
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
uDateParse s = Ok (Year 2011)

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
