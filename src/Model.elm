module Model exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import UDate exposing (..)

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

resumeDecoder : Decoder Resume
resumeDecoder =
  decode Resume
    |> required "name" string
    |> required "contact" contactDecoder
    |> optional "socialMedia" (list socialMediaWithPriorityDecoder) []
    |> optional "sections" (list sectionDecoder) []

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
  , body : Body
  }

skillItemDecoder : Decoder SkillItem
skillItemDecoder =
  decode SkillItem
    |> required "title" string
    |> required "body" bodyDecoder

type alias ExperienceItem =
  { title : String
  , body : Body
  , begin : UDate
  , end : Maybe UDate
  }

experienceItemDecoder : Decoder ExperienceItem
experienceItemDecoder =
  decode ExperienceItem
    |> required "title" string
    |> required "body" bodyDecoder
    |> required "begin" uDateDecoder
    |> optional "end" (oneOf
      [ null Nothing
      , (custom uDateDecoder <| decode Just)
      ]) Nothing

type Body
  = Text String
  | Skills (List SkillItem)
  | Experiences (List ExperienceItem)

bodyDecoder : Decoder Body
bodyDecoder =
  let
    textDecoder = map Text string
    skillsDecoder = decode Skills |> required "skills" (list (lazy (\_ -> skillItemDecoder)))
    experienceDecoder = decode Experiences |> required "experiences" (list (lazy (\_ -> experienceItemDecoder)))
  in
    oneOf
      [ textDecoder
      , skillsDecoder
      , experienceDecoder
      ]

type alias Section =
  { title : String
  , body : Body
  }

sectionDecoder : Decoder Section
sectionDecoder =
  decode Section
    |> required "title" string
    |> required "body" bodyDecoder
