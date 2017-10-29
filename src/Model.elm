module Model exposing (..)

import RemoteData exposing (WebData, RemoteData(..))
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import UDate exposing (..)
import Config exposing (Config)
import Result.Extra exposing (combine)

type alias Model =
  { resume : WebData Resume
  , config : Config
  }

initModel : Config -> Model
initModel config =
  { resume = NotAsked
  , config = config
  }

type alias Resume =
  { name : String
  , contact : Contact
  , socialMedia : List SocialMedia
  , secondarySocialMedia : List SocialMedia
  , sections : List Section
  }

resumeDecoder : Decoder Resume
resumeDecoder =
  decode Resume
    |> required "name" string
    |> required "contact" contactDecoder
    |> optional "socialMedia" socialMediaDecoder []
    |> optional "secondarySocialMedia" socialMediaDecoder []
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
  = Mandatory
  | Optional String

priorityDecoder : Decoder Priority
priorityDecoder =
  let
    match s = case s of
      "" -> succeed Mandatory
      name -> succeed (Optional name)
  in
    andThen match
      (decode identity |>
        optional "optional" string "")

type SocialMedia
  = Twitter String
  | Github String
  | GTalk String
  | Skype String
  | LinkedIn String
  | StackOverflow String

socialMediaDecoder : Decoder (List SocialMedia)
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
    mapMedia (key, value) =
      case key of
        "github" ->         Ok <| Github value
        "gtalk" ->          Ok <| GTalk value
        "twitter" ->        Ok <| Twitter value
        "skype" ->          Ok <| Skype value
        "linkedin" ->       Ok <| LinkedIn value
        "stackoverflow" ->  Ok <| StackOverflow value
        invalid ->          Err <| "No match for social media handler: " ++ invalid
    mapAllMedia : List (String, String) -> Result String (List SocialMedia)
    mapAllMedia ls =
      combine (List.map mapMedia ls)
    mapAllMediaDecoder : Result String (List SocialMedia) -> Decoder (List SocialMedia)
    mapAllMediaDecoder r = resultToDecoder r
  in
    Json.Decode.andThen (mapAllMediaDecoder << mapAllMedia) (keyValuePairs string)
    -- oneOf
    --   [ matchValue "github"        Github        string
    --   , matchValue "gtalk"         GTalk         string
    --   , matchValue "twitter"       Twitter       string
    --   , matchValue "skype"         Skype         string
    --   , matchValue "linkedin"      LinkedIn      string
    --   , matchValue "stackoverflow" StackOverflow string
    --   ]

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

resultToDecoder : Result String a -> Decoder a
resultToDecoder r = case r of
  Err e -> fail e
  Ok v -> succeed v
