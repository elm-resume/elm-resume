module Action exposing (..)

import Model exposing (Resume)
import RemoteData exposing (WebData)

type Action
  = RequestedData (WebData Resume)
