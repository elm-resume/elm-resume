module ResumeState exposing (..)

import Resume exposing (..)
import Set exposing (Set)

type alias ResumeState =
  { name : String
  , contact : Contact
  , socialMedia : List SocialMedia
  , optionalSocialMedia : { visible : Bool, handles : List SocialMedia }
  , sections : List Section
  , visible : Set Id
  }

type alias Id = String
