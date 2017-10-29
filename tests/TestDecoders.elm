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
        testDecode """{ "name" : "Jane", "contact" : { "address" : "", "email" : "", "phone" : "" } }""" resumeDecoder { name = "Jane", contact = { address = "", email = "", phone = "" }, socialMedia = [], secondarySocialMedia = [], sections = [] }
      ]
    , describe "Contact decoders"
      [ test "Contact" <|
        testDecode """{ "address" : "a", "email" : "e", "phone" : "p" }""" contactDecoder { address = "a", email = "e", phone = "p" }
      ]
    , describe "Priority decoders"
      [ test "Mandatory" <|
        testDecode """{}""" priorityDecoder Mandatory
      , test "Optional" <|
        testDecode """{ "optional" : "label" }""" priorityDecoder (Optional "label")
      ]
    , describe "SocialMedia decoders"
      [ test "Twitter Secondary" <|
        testDecode """{ "twitter" : "t" }""" socialMediaDecoder [Twitter "t"]
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
