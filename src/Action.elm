module Action exposing (..)

import RemoteData exposing (WebData)
import Resume exposing (Resume)
import ResumeState exposing (Id)

type Action
  = RequestedData (WebData Resume)
  | ToggleOptionalSocialMedia
  | ToggleItem Id
