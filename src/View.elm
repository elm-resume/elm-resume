module View exposing (view)

import Html exposing (..)
import Markdown
import Html.Attributes exposing (..)
import RemoteData exposing (RemoteData(..))
import Model exposing (..)

view : Model -> Html a
view { resume } =
  case resume of
    Success r -> viewResume r
    Failure msg -> div [] [text msg]
    _ -> div [] []

viewResume : Resume -> Html a
viewResume { name, contact, socialMedia, sections } =
  div
    [ class "resume" ] <|
    [ h1 [ class "resume-name" ] [ text name ]
    , div
        [ class "resume-contact-section" ]
        [ viewContact contact
        , viewSocialMediaList socialMedia
        ]
    ] ++ (List.map viewSection sections)

viewContact : Contact -> Html a
viewContact { address, email, phone } =
  div [ class "resume-address" ]
    [ node "address" [ class "resume-address-street" ] [ Markdown.toHtml [] address ]
    , a [ class "resume-address-email", href <| "mailto:" ++ email ] [ text email ]
    , a [ class "resume-address-phone", href <| "tel:" ++ phone ] [ text phone ]
    ]

viewSocialMediaList : List SocialMediaWithPriority -> Html a
viewSocialMediaList =
  div [ class "resume-social" ] << List.filterMap viewSocialMedia

-- TODO icons
viewSocialMedia : SocialMediaWithPriority -> Maybe (Html a)
viewSocialMedia { handle, priority } =
  case (priority, handle) of
    (Secondary, _) -> Nothing
    (Primary, Twitter v) -> Just <|
      a [ class "resume-social-link", href <| "http://twitter.com/" ++ v ]
        [ text v ]
    (Primary, Github v) -> Just <|
      a [ class "resume-social-link", href <| "http://github.com/" ++ v ]
        [ text v ]
    (Primary, Skype v) -> Just <|
      a [] [ text v ] -- TODO
    (Primary, LinkedIn v) -> Just <|
      a [ class "resume-social-link", href <| "http://linkedin.com/in/" ++ v ]
        [ text v ]
    (Primary, StackOverflow v) -> Just <| -- TODO: string should be int here
      a [ class "resume-social-link", href <| "http://stackoverflow.com/users/" ++ v ]
        [ text v ]

viewSection : Section -> Html a
viewSection { title, body } =
  div [ class "resume-section" ]
    [ h1 [ class "resume-section-title" ] [ text title ]
    , viewBody body
    ]

viewBody : Body -> Html a
viewBody body =
  let
    content : Html a
    content =
      case body of
        Text v ->
          Markdown.toHtml [] v
        Skills v ->
          ul [ class "resume-skill-items" ] <| List.map viewSkillItem v
        Experiences v ->
          ul [ class "resume-experience-items" ] <| List.map viewExperienceItem v
  in
    div [ class "resume-section-body" ] [ content ]

viewSkillItem : SkillItem -> Html a
viewSkillItem { title, body } =
  li [ class "resume-skill-item" ] [ text title ] -- TODO


viewExperienceItem : ExperienceItem -> Html a
viewExperienceItem { title, body, begin, end } =
  li [ class "resume-skill-item" ] [ text title ] -- TODO
