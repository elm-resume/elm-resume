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
        testDecode """{ "type" : "twitter", "value" : "t" }""" contactDecoder (Twitter "t")
      , test "Skype" <|
        testDecode """{ "type" : "skype", "value" : "s" }""" contactDecoder (Skype "s")
      , test "Github" <|
        testDecode """{ "type" : "github", "value" : "g" }""" contactDecoder (Github "g")
      ]
    , describe "ContactWithPrioriy decoders"
      [ test "Twitter Secondary" <|
        testDecode """{ "type" : "twitter", "value" : "t", "prio" : "secondary" }""" contactWithPriorityDecoder ({ contact = Twitter "t", priority = Secondary })
      , test "Twitter Primary" <|
        testDecode """{ "type" : "twitter", "value" : "t", "prio" : "primary" }""" contactWithPriorityDecoder ({ contact = Twitter "t", priority = Primary })
      , test "Twitter Default" <|
        testDecode """{ "type" : "twitter", "value" : "t" }""" contactWithPriorityDecoder ({ contact = Twitter "t", priority = Primary })
      ]
    ]

assertDecode : String -> Decoder a -> a -> Expectation
assertDecode s d a =
  let
    decoded = decodeString d s
  in
    case decoded of
      Ok value -> Expect.equal a value
      Err f -> Expect.fail f

testDecode : String -> Decoder a -> a -> (() -> Expectation)
testDecode s d a = \() -> assertDecode s d a
