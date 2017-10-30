module Resume exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import UDate exposing (..)
import Result.Extra exposing (combine)
import Debug

type alias Resume =
  { name : String
  , contact : Contact
  , socialMedia : List SocialMedia
  , optionalSocialMedia : List SocialMedia
  , sections : List Section
  }

resumeDecoder : Decoder Resume
resumeDecoder =
  decode Resume
    |> required "name" string
    |> required "contact" contactDecoder
    |> optional "socialMedia" socialMediaDecoder []
    |> optional "optionalSocialMedia" socialMediaDecoder []
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
  | OpenHub String
  | Website String

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
        "ohloh" ->          Ok <| OpenHub value
        "openhub" ->        Ok <| OpenHub value
        "website" ->        Ok <| Website value
        invalid ->          Err <| "No match for social media handler: " ++ invalid
    mapAllMedia : List (String, String) -> Result String (List SocialMedia)
    mapAllMedia ls =
      combine (List.map mapMedia <| List.reverse ls)
    mapAllMediaDecoder : Result String (List SocialMedia) -> Decoder (List SocialMedia)
    mapAllMediaDecoder r = resultToDecoder r
  in
    Json.Decode.andThen (mapAllMediaDecoder << mapAllMedia) (keyValuePairs string)

type DateRange
  = Between UDate UDate
  | After UDate
  | Undetermined

dateRangeDecoder : Decoder DateRange
dateRangeDecoder =
  let
    between =
      decode Between
        |> required "begin" uDateDecoder
        |> required "end" uDateDecoder
    after =
      decode After
        |> required "begin" uDateDecoder
    undetermined =
      succeed Undetermined
  in
    oneOf
      [ between
      , after
      , undetermined
      ]

type alias Item =
  { title : String
  , body  : Maybe Body
  , dates : DateRange
  , prio : Priority
  }

maybeField : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybeField s d =
  optional s (custom d <| decode Just) Nothing

itemDecoder : Decoder Item
itemDecoder =
  let
    half : Decoder (DateRange -> Priority -> Item)
    half = decode Item
      |> required   "title" string
      |> maybeField "body"  bodyDecoder
    withDate =
      andThen (\f -> map f dateRangeDecoder) half
    withPrio =
      andThen (\f -> map f priorityDecoder) withDate
  in
    withPrio

type Body
  = Text String
  | Items (Maybe String) (List Item) -- TODO

bodyDecoder : Decoder Body
bodyDecoder =
  let
    textDecoder =
      map Text string
    contentDecoder =
      decode identity
        |> maybeField "content" string
    experienceDecoder =
      andThen (\v -> decode (Items v)
        |> required "items" (list (lazy (\_ -> itemDecoder)))) contentDecoder
  in
    oneOf
      [ experienceDecoder
      , textDecoder
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
