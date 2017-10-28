module TestUDate exposing (..)

import Common exposing (..)
import Expect exposing (equal)
import Test exposing (..)
import UDate exposing (..)

suite : Test
suite =
  describe "Resume Tests"
    [ describe "Test UDate building"
      [ test "Year" <|
        \() -> equal (uDateToString (year 2017)) "2017"
      , test "Month" <|
        \() -> equal (uDateToString (month 2017 10)) "2017-10"
      , test "Month out of range (positive)" <|
        \() -> equal (uDateToString (month 2015 25)) "2017-01"
      , test "Month out of range (zero)" <|
        \() -> equal (uDateToString (month 2018 0)) "2017-12"
      , test "Month out of range (negative)" <|
        \() -> equal (uDateToString (month 2018 -2)) "2017-10"
      , test "Day" <|
        \() -> equal (uDateToString (day 2017 10 28)) "2017-10-28"
      , test "Day out of range (positive)" <|
        \() -> equal (uDateToString (day 2017 2 29)) "2017-03-01"
      , test "Day out of range (zero)" <|
        \() -> equal (uDateToString (day 2018 0 0)) "2017-11-30"
      , test "Day out of range (negative)" <|
        \() -> equal (uDateToString (day 2018 1 -2)) "2017-12-29"
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
