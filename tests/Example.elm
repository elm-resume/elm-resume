module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Json.Decode exposing (Decoder, decodeString)
import Model exposing (..)

suite : Test
suite =
  describe "Resume Tests"
    [ describe "Priority decoders"
      [ test "Primary" <|
        testDecode "\"primary\"" priorityDecoder Primary
      , test "Secondary" <|
        testDecode "\"secondary\"" priorityDecoder Secondary
      ]
    , describe "Contact decoders"
      [ test "Twitter" <|
        testDecode """{ "type" : "twitter", "value" : "t" }""" socialMediaDecoder (Twitter "t")
      , test "Skype" <|
        testDecode """{ "type" : "skype", "value" : "s" }""" socialMediaDecoder (Skype "s")
      , test "Github" <|
        testDecode """{ "type" : "github", "value" : "g" }""" socialMediaDecoder (Github "g")
      ]
    , describe "ContactWithPrioriy decoders"
      [ test "Twitter Secondary" <|
        testDecode """{ "type" : "twitter", "value" : "t", "prio" : "secondary" }""" socialMediaWithPriorityDecoder ({ handle = Twitter "t", priority = Secondary })
      , test "Twitter Primary" <|
        testDecode """{ "type" : "twitter", "value" : "t", "prio" : "primary" }""" socialMediaWithPriorityDecoder ({ handle = Twitter "t", priority = Primary })
      , test "Twitter Default" <|
        testDecode """{ "type" : "twitter", "value" : "t" }""" socialMediaWithPriorityDecoder ({ handle = Twitter "t", priority = Primary })
      ]
    , describe "Test UDate building"
      [ test "Year" <|
        \() -> Expect.equal (uDateToString (year 2017)) "2017"
      , test "Month" <|
        \() -> Expect.equal (uDateToString (month 2017 10)) "2017-10"
      , test "Month out of range (positive)" <|
        \() -> Expect.equal (uDateToString (month 2015 25)) "2017-01"
      , test "Month out of range (zero)" <|
        \() -> Expect.equal (uDateToString (month 2018 0)) "2017-12"
      , test "Month out of range (negative)" <|
        \() -> Expect.equal (uDateToString (month 2018 -2)) "2017-10"
      , test "Day" <|
        \() -> Expect.equal (uDateToString (day 2017 10 28)) "2017-10-28"
      , test "Day out of range (positive)" <|
        \() -> Expect.equal (uDateToString (day 2017 2 29)) "2017-03-01"
      , test "Day out of range (zero)" <|
        \() -> Expect.equal (uDateToString (day 2018 0 0)) "2017-11-30"
      , test "Day out of range (negative)" <|
        \() -> Expect.equal (uDateToString (day 2018 1 -2)) "2017-12-29"
      ]
    , describe "Test UDate parsing"
      [ test "Year" <|
        testResult (uDateParse "2017") (year 2017)
      , test "Month" <|
        testResult (uDateParse "2017-10") (month 2017 10)
      , test "Day" <|
        testResult (uDateParse "2017-10-28") (day 2017 10 28)
      ]
    ]

assertResult : Result String a -> a -> Expectation
assertResult r a =
  case r of
      Ok value -> Expect.equal a value
      Err f -> Expect.fail f

expectErrResult : Result String a -> Expectation
expectErrResult r =
  case r of
      Ok value -> Expect.fail <| "result was expected to be error but it is Ok with: " ++ (toString value)
      Err f -> Expect.pass

assertDecode : String -> Decoder a -> a -> Expectation
assertDecode s d a =
  let
    decoded = decodeString d s
  in
    assertResult decoded a

testDecode : String -> Decoder a -> a -> (() -> Expectation)
testDecode s d a = \() -> assertDecode s d a

testResult : Result String a -> a -> (() -> Expectation)
testResult r a = \() -> assertResult r a

testErrResult : Result String a -> (() -> Expectation)
testErrResult r = \() -> expectErrResult r
