module TestDecoders exposing (..)

import Common exposing (..)
import Test exposing (..)
import Model exposing (..)
import UDate exposing (year, month)

suite : Test
suite =
  describe "Resume Tests"
    [ describe "Resume decoder"
      [ test "Resume" <|
        testDecode """{ "name" : "Jane", "contact" : { "address" : "", "email" : "", "phone" : "" }, "socialMedia" : [], "sections" : [] }""" resumeDecoder { name = "Jane", contact = { address = "", email = "", phone = "" }, socialMedia = [], sections = [] }
      ]
    , describe "Contact decoders"
      [ test "Twitter" <|
        testDecode """{ "type" : "twitter", "value" : "t" }""" socialMediaDecoder (Twitter "t")
      , test "Skype" <|
        testDecode """{ "type" : "skype", "value" : "s" }""" socialMediaDecoder (Skype "s")
      , test "Github" <|
        testDecode """{ "type" : "github", "value" : "g" }""" socialMediaDecoder (Github "g")
      ]
    , describe "Priority decoders"
      [ test "Primary" <|
        testDecode "\"primary\"" priorityDecoder Primary
      , test "Secondary" <|
        testDecode "\"secondary\"" priorityDecoder Secondary
      ]
    , describe "SocialMediatWithPrioriy decoders"
      [ test "Twitter Secondary" <|
        testDecode """{ "type" : "twitter", "value" : "t", "prio" : "secondary" }""" socialMediaWithPriorityDecoder { handle = Twitter "t", priority = Secondary }
      , test "Twitter Primary" <|
        testDecode """{ "type" : "twitter", "value" : "t", "prio" : "primary" }""" socialMediaWithPriorityDecoder { handle = Twitter "t", priority = Primary }
      , test "Twitter Default" <|
        testDecode """{ "type" : "twitter", "value" : "t" }""" socialMediaWithPriorityDecoder { handle = Twitter "t", priority = Primary }
      ]
    , describe "SkillItem decoders"
      [ test "SkillItem with text body" <|
        testDecode """{ "title" : "TITLE", "body" : "hi!" }""" skillItemDecoder { title = "TITLE", body = Text "hi!" }
      ]
    , describe "ExperienceItem decoders"
      [ test "Experience with text body" <|
        testDecode """{ "title" : "TITLE", "body" : "hi!", "begin" : "2007" }""" experienceItemDecoder { title = "TITLE", body = Text "hi!", begin = year 2007, end = Nothing }
      , test "Experience with text body and `end` date" <|
        testDecode """{ "title" : "TITLE", "body" : "hi!", "begin" : "2007", "end" : "2010-10" }""" experienceItemDecoder { title = "TITLE", body = Text "hi!", begin = year 2007, end = Just (month 2010 10) }
      ]
    , describe "Body decoders"
      [ test "Body Text" <|
        testDecode "\"content\"" bodyDecoder (Text "content")
      , test "Skills Text" <|
        testDecode """{ "skills" : [] }""" bodyDecoder (Skills [])
      , test "Experience Text" <|
        testDecode """{ "experiences" : [] }""" bodyDecoder (Experiences [])
      ]
    , describe "Section decoders"
      [ test "Section with text body" <|
        testDecode """{ "title" : "TITLE", "body" : "hi!" }""" sectionDecoder { title = "TITLE", body = Text "hi!" }
      ]
    ]
