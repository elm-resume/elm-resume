module UDate exposing (UDate, year, month, day, uDateDecoder, uDateParse, uDateToString, isLeapYear, daysInMonth)

import Array exposing (..)
import Json.Decode exposing (..)
import Regex exposing (..)

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
    uDatePattern = regex "^(\\d{4})(?:[-](\\d{2}))?(?:[-](\\d{2}))?$"
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
  oneOf [
    Json.Decode.map year int,
    andThen (resultToDecoder << uDateParse) string
  ]

isLeapYear : Int -> Bool
isLeapYear y = y % 4 == 0 && (y % 100 /= 0 || y % 400 == 0)

daysToMonth365_ : Array Int
daysToMonth365_ = fromList [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
daysToMonth366_ : Array Int
daysToMonth366_ = fromList [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366]

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
    Maybe.withDefault 0 <| Maybe.map2 (\a b -> a - b) (get n.m days) (get (n.m - 1) days)
