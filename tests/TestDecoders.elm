module TestDecoders exposing (..)

import Common exposing (..)
import Test exposing (..)
import Resume exposing (..)
import UDate exposing (year, month)

suite : Test
suite =
  describe "Resume Tests"
    [ describe "Resume decoder"
      [ test "Resume" <|
        testDecode """{ "name" : "Jane", "contact" : { "address" : "", "email" : "", "phone" : "" } }""" resumeDecoder { name = "Jane", contact = { address = "", email = "", phone = "" }, socialMedia = [], optionalSocialMedia = [], sections = [] }
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
      [ test "Twitter" <|
        testDecode """{ "twitter" : "t" }""" socialMediaDecoder [Twitter "t"]
      ]
    , describe "Item decoders"
      [ test "Item with no dates" <|
        testDecode """{ "title" : "TITLE", "content" : "hi!" }""" itemDecoder { title = "TITLE", body = ContentOnly "hi!", prio = Mandatory, dates = Undetermined }
      , test "Item" <|
        testDecode """{ "title" : "TITLE", "content" : "hi!", "begin" : "2007" }""" itemDecoder { title = "TITLE", body = ContentOnly "hi!", dates = After <| year 2007, prio = Mandatory }
      , test "Item with text body and `end` date" <|
        testDecode """{ "title" : "TITLE", "content" : "hi!", "begin" : "2007", "end" : "2010-10", "optional" : "blah" }""" itemDecoder { title = "TITLE", body = ContentOnly "hi!", dates = Between (year 2007) (month 2010 10), prio = Optional "blah" }
      ]
    , describe "Body decoders"
      [ test "Items" <|
        testDecode """{ "items" : [] }""" bodyDecoder (ItemsOnly [])
      ]
    , describe "Section decoders"
      [ test "Section with text body" <|
        testDecode """{ "title" : "TITLE", "content" : "hi!" }""" sectionDecoder { title = "TITLE", body = ContentOnly "hi!" }
      ]
    ]
