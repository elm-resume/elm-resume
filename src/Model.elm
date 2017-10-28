module Model exposing (Model, initModel, Resume, resumeDecoder, Contact, contactDecoder, Priority(..), priorityDecoder, SocialMedia(..), socialMediaDecoder, SocialMediaWithPriority, socialMediaWithPriorityDecoder, SkillItem, skillItemDecoder, ExperienceItem, experienceItemDecoder, SectionBody, sectionBodyDecoder, Section, sectionDecoder, UDate, year, month, day, uDateDecoder, uDateParse, uDateToString, isLeapYear, daysInMonth)

import Array
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Regex exposing (..)

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

year : Int -> UDate
year y = Year y

month_ : Int -> Int -> { y : Int, m : Int }
month_ y m =
  let
    dy = floor ((toFloat m - 1) / 12)
    dm = ((m - 1) % 12) + 1
  in
    { y = y + dy, m = dm }

month : Int -> Int -> UDate
month y m =
  let
    n = month_ y m
  in
    Month n.y n.m

isLeapYear : Int -> Bool
isLeapYear y = y % 4 == 0 && (y % 100 /= 0 || y % 400 == 0)

daysToMonth365_ = Array.fromList [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
daysToMonth366_ = Array.fromList [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366]

daysInMonth : Int -> Int -> Int
daysInMonth y m =
  let
    n = month_ y m
    days = case isLeapYear n.y of
      True ->
        daysToMonth366_
      False ->
        daysToMonth365_
  in
    Maybe.withDefault 0 <| Maybe.map2 (\a b -> a - b) (Array.get n.m days) (Array.get (n.m - 1) days)

day : Int -> Int -> Int -> UDate
day y m d =
  let
    n = month_ y m
    dm = daysInMonth n.y n.m
  in
    if d > dm then
      day n.y (n.m + 1) (d - dm)
    else if d <= 0 then
      day n.y (n.m - 1) (d + (daysInMonth n.y (n.m - 1)))
    else
      Day n.y n.m d

uDateToString : UDate -> String
uDateToString d =
  let
    f i = Basics.toString i
    f2 i = case i < 10 of
      True -> "0" ++ (f i)
      False -> f i
    fd i = "-" ++ f2 i
  in
    case d of
      Year y -> f y
      Month y m -> f y ++ fd m
      Day y m d -> f y ++ fd m ++ fd d

uDateParse : String -> Result String UDate
uDateParse s =
  let
    uDatePattern = regex "^([0-9]{4})(?:[-]([0-9]{2}))?(?:[-]([0-9]{2}))?$"
    matches = find All uDatePattern s
  in
    case matches of
      [{ submatches }] ->
        case submatches of
          [Just sy, Nothing, Nothing] ->
            Result.map year (String.toInt sy)
          [Just sy, Just sm, Nothing] ->
            Result.map2 month (String.toInt sy) (String.toInt sm)
          [Just sy, Just sm, Just sd] ->
            Result.map3 day (String.toInt sy) (String.toInt sm) (String.toInt sd)
          _ ->
            Err <| "Failed to parse date: " ++ s
      _ ->
        Err <| "Failed to parse date: " ++ s

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
