module Common exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Json.Decode exposing (Decoder, decodeString)

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
