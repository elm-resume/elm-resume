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
import Set exposing (Set)
import UDate exposing (..)
import Json.Decode

view : Model -> Html Action
view { resume } =
  case resume of
    Success r -> viewResume r
    Failure msg -> div [style [("font-family", "monospace")]] [markdown <| toString msg]
    _ -> div [] []

onClickPrevendDefault : msg -> Attribute msg
onClickPrevendDefault msg =
  onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.Decode.succeed msg)

expand : Action -> Html Action
expand action =
  a [ href "#", onClickPrevendDefault action, class "toggle expand" ]
    [ span [ class "text" ] [text "expand"] ]
collapse : Action -> Html Action
collapse action =
  a [ href "#", onClickPrevendDefault action, class "toggle collapse" ]
    [ span [ class "text" ] [text "collapse"] ]

viewResume : ResumeState -> Html Action
viewResume { name, contact, socialMedia, optionalSocialMedia, sections, visible, intro, headline } =
  div
    [ class "resume" ] <|
    [ h1 [ class "resume-name" ]
      [ span [] [text name]
      , Maybe.withDefault (text "") (Maybe.map (\v -> span [ class "resume-headline" ] [ text v ]) headline)
      ]
    , div
        [ class "resume-contact-section" ]
        ([ viewContact contact
        , viewSocialMediaList socialMedia
        ]
        ++ (
          if List.isEmpty optionalSocialMedia.handles then
            []
          else
            if optionalSocialMedia.visible then
              [ viewSocialMediaList optionalSocialMedia.handles
              , collapse ToggleOptionalSocialMedia
              ]
            else
              [ expand ToggleOptionalSocialMedia ]
        ))
    ]
     ++ [viewIntro intro]
     ++ (List.map (viewSection visible) sections)
     ++ [ div
            [ class "resume-footer"]
            [ div
                [ class "footer" ]
                [ div [ class "left" ] [ text name ]
                , div [ class "center" ] [ text contact.email ]
                , div [ class "right" ] [ text contact.phone ]
                ]
            ]
        ]

viewIntro : Maybe String -> Html Action
viewIntro maybeIntro =
  case maybeIntro of
    Nothing -> text ""
    Just intro ->
      div
        [ class "resume-intro" ]
        [ markdown intro ]

viewContact : Contact -> Html Action
viewContact { address, email, phone } =
  div [ class "resume-address" ]
    [ node "address" [ class "resume-address-street" ] [ markdown address ]
    , a [ class "resume-address-email", href <| "mailto:" ++ email ] [ text email ]
    , a [ class "resume-address-phone", href <| "tel:" ++ phone ] [ text phone ]
    ]

viewSocialMediaList : List SocialMedia -> Html Action
viewSocialMediaList =
  div [ class "resume-social" ] << List.map viewSocialMedia

viewSocialMedia : SocialMedia -> Html Action
viewSocialMedia handle =
  case handle of
    Twitter v ->
      a [ class "resume-social-link twitter", href <| "http://twitter.com/" ++ v ]
        [ text v ]
    Github v ->
      a [ class "resume-social-link github", href <| "http://github.com/" ++ v ]
        [ text v ]
    Skype v ->
      span [ class "resume-social-link skype" ] [ text v ]
    GTalk v ->
      span [ class "resume-social-link gtalk" ] [ text v ]
    OpenHub v ->
      a [ class "resume-social-link open-hub", href <| "https://www.openhub.net/accounts/" ++ v ]
        [ text v ]
    Website v ->
      a [ class "resume-social-link website", href <| v ]
        [ text v ]
    LinkedIn v ->
      a [ class "resume-social-link linkedin", href <| "http://linkedin.com/in/" ++ v ]
        [ text v ]
    StackOverflow v -> -- TODO: string should be int here
      a [ class "resume-social-link stack-overflow", href <| "http://stackoverflow.com/users/" ++ v ]
        [ text v ]

viewSection : Set Id -> Section -> Html Action
viewSection visibles { title, body } =
  div [ class "resume-section" ]
    [ h2 [ class "resume-section-title" ] [ text title ]
    , viewBody visibles body
    ]

viewCollapsible : Set Id -> Maybe String -> (() -> Html Action) -> Html Action
viewCollapsible ids maybeId render =
  case maybeId of
    Nothing ->
      render ()
    Just id ->
      if Set.member id ids then
        div
          []
          [ collapse <| ToggleItem id
          , render ()
          ]
      else
        div
          []
          [ expand <| ToggleItem id
          ]

viewBody : Set Id -> Body -> Html Action
viewBody visibles body =
  let
    content : Html Action
    content =
      case body of
        Empty ->
          text ""
        ContentOnly v ->
          markdown v
        ItemsOnly v opt ->
          viewCollapsible visibles opt (\() ->
            ul [ class "resume-items" ]
              <| List.map (viewItem visibles) v
          )
        ContentAndItems c v opt ->
          div
            []
            [ markdown c
            , viewCollapsible visibles opt (\() ->
                ul [ class "resume-items" ]
                  <| List.map (viewItem visibles) v
              )
            ]
  in
    div [ class "resume-section-body" ] [ content ]

viewDateRange : DateRange  -> Html Action
viewDateRange dates =
  let content =
    case dates of
      Between begin end ->
        if begin == end then
          text <| (uDateToUSString begin)
        else
          text <| (uDateToUSString begin) ++ " to " ++ (uDateToUSString end)
      After begin ->
        text <| (uDateToUSString begin) ++ " to present"
      Undetermined ->
        text ""
  in
    span [ class "date" ] [ content ]

viewContext : String -> Html Action
viewContext ctx =
  span
    [ class "item-title-context" ]
    [ text ctx ]

viewItem : Set Id -> Item -> Html Action
viewItem visibles { title, context, body, dates, prio } =
  let
    content =
      div
        []
        [ header [ class "title-and-date" ]
          [ h3
              [ class "resume-section-item-title" ]
              [ span
                  [ class "item-title-text" ]
                  [ text title ]
              , Maybe.withDefault (text "") <| Maybe.map viewContext context]
          , viewDateRange dates
          ]
        , viewBody visibles body
        ]
  in
    case prio of
      Mandatory ->
        li
          [ class "resume-skill-item" ]
          [ content ]
      Optional id ->
        if isVisible visibles prio then
          li
            [ class "resume-skill-item" ]
            [ collapse (ToggleItem id)
            , content
            ]
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

markdown : String -> Html Action
markdown s = Markdown.toHtml [ class "markdown" ] s

-- TODO header / footer
