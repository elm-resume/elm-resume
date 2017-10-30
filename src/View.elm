module View exposing (view)

import Html exposing (..)
import Markdown
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..))
import Model exposing (..)
import Resume exposing (..)
import ResumeState exposing (..)
import Action exposing (..)
import Maybe.Extra exposing (toList)
import Set exposing (Set)
import UDate exposing (..)

view : Model -> Html Action
view { resume } =
  case resume of
    Success r -> viewResume r
    Failure msg -> div [style [("font-family", "monospace")]] [Markdown.toHtml [] <| toString msg]
    _ -> div [] []

expand : Action -> Html Action
expand action =
  button [ onClick action ] [ text "expand" ]
collapse : Action -> Html Action
collapse action =
  button [ onClick action ] [ text "collapse" ]

viewResume : ResumeState -> Html Action
viewResume { name, contact, socialMedia, optionalSocialMedia, sections, visible } =
  -- TODO visible
  div
    [ class "resume" ] <|
    [ h1 [ class "resume-name" ] [ text name ]
    , div
        [ class "resume-contact-section" ]
        ([ viewContact contact
        , viewSocialMediaList socialMedia
        ]
        ++ (
          if optionalSocialMedia.visible then
            [ collapse ToggleOptionalSocialMedia
            , viewSocialMediaList optionalSocialMedia.handles
            ]
          else
            [ expand ToggleOptionalSocialMedia ]
        ))
    ] ++ (List.map (viewSection visible) sections)

viewContact : Contact -> Html Action
viewContact { address, email, phone } =
  div [ class "resume-address" ]
    [ node "address" [ class "resume-address-street" ] [ Markdown.toHtml [] address ]
    , a [ class "resume-address-email", href <| "mailto:" ++ email ] [ text email ]
    , a [ class "resume-address-phone", href <| "tel:" ++ phone ] [ text phone ]
    ]

viewSocialMediaList : List SocialMedia -> Html Action
viewSocialMediaList =
  div [ class "resume-social" ] << List.filterMap viewSocialMedia

-- TODO icons
viewSocialMedia : SocialMedia -> Maybe (Html Action)
viewSocialMedia handle =
  case handle of
    Twitter v -> Just <|
      a [ class "resume-social-link twitter", href <| "http://twitter.com/" ++ v ]
        [ text v ]
    Github v -> Just <|
      a [ class "resume-social-link github", href <| "http://github.com/" ++ v ]
        [ text v ]
    Skype v -> Just <|
      span [ class "resume-social-item skype" ] [ text v ]
    GTalk v -> Just <|
      span [ class "resume-social-item gtalk" ] [ text v ]
    OpenHub v -> Just <|
      a [ class "resume-social-link open-hub", href <| "https://www.openhub.net/accounts/" ++ v ]
        [ text v ]
    Website v -> Just <|
      a [ class "resume-social-link website", href <| v ]
        [ text v ]
    LinkedIn v -> Just <|
      a [ class "resume-social-link linkedin", href <| "http://linkedin.com/in/" ++ v ]
        [ text v ]
    StackOverflow v -> Just <| -- TODO: string should be int here
      a [ class "resume-social-link stack-ovrflow", href <| "http://stackoverflow.com/users/" ++ v ]
        [ text v ]

viewSection : Set Id -> Section -> Html Action
viewSection visibles { title, body } =
  div [ class "resume-section" ]
    [ h1 [ class "resume-section-title" ] [ text title ]
    , viewBody visibles body
    ]

viewContent : Maybe String -> List (Html Action)
viewContent m = case m of
  Nothing -> []
  Just s -> [Markdown.toHtml [] s]

viewBody : Set Id -> Body -> Html Action
viewBody visibles body =
  let
    content : Html Action
    content =
      case body of
        Text v ->
          Markdown.toHtml [] v
        Items c v ->
          div [] <| viewContent c ++ [
            ul [ class "resume-items" ]
              <| List.map (mapItem visibles) v
          ]
  in
    div [ class "resume-section-body" ] [ content ]

viewMaybeBody : Set Id -> Maybe Body -> List (Html Action)
viewMaybeBody visibles m = toList <| Maybe.map (viewBody visibles) m

mapItem : Set Id -> Item -> Html Action
mapItem visibles item =
  viewItem_ visibles item.prio (\() -> viewItem visibles item)

viewDateRange : DateRange  -> Html Action
viewDateRange dates =
  case dates of
    Between begin end ->
      text <| (uDateToString begin) ++ " to " ++ (uDateToString end)
    After begin ->
      text <| (uDateToString begin) ++ " to present"
    Undetermined ->
      text ""

viewItem : Set Id -> Item -> Html Action
viewItem visibles { title, body, dates, prio } =
  div
    [ class "" ]
    (
      [ h2 [] [text title]
      , viewDateRange dates
      ] ++ viewMaybeBody visibles body)

viewItem_ : Set Id -> Priority -> (() -> Html Action) -> Html Action
viewItem_ visibles prio render =
  case prio of
    Mandatory ->
      li
        [ class "resume-skill-item" ]
        [ render () ]
    Optional id ->
      if isVisible visibles prio then
        li
          [ class "resume-skill-item" ]
          [ collapse (ToggleItem id)
          , render () ]
      else
        li
          [ class "resume-skill-item-expand" ]
          [ expand (ToggleItem id) ]

isVisible : Set Id -> Priority -> Bool
isVisible visibles prio =
  case prio of
    Mandatory -> True
    Optional id -> Set.member id visibles

canToggle : Priority -> Bool
canToggle prio =
  case prio of
    Mandatory -> False
    Optional _ -> True
