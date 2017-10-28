module TestDecoders exposing (..)

import Common exposing (..)
import Test exposing (..)
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
    ]
