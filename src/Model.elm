module Model exposing (..)

import RemoteData exposing (WebData, RemoteData(..))
import Config exposing (Config)
import Resume exposing (Resume)
import ResumeState exposing (ResumeState)
import Set exposing (empty)

type alias Model =
  { resume : WebData ResumeState
  , config : Config
  }

initModel : Config -> Model
initModel config =
  { resume = NotAsked
  , config = config
  }

resumeToResumeState : Resume -> ResumeState
resumeToResumeState r =
  { name = r.name
  , contact = r.contact
  , socialMedia = r.socialMedia
  , optionalSocialMedia = { visible = False, handles = r.optionalSocialMedia }
  , sections = r.sections
  , visible = empty
  }
