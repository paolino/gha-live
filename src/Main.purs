module Main where

import Prelude

import Data.Array (filter, index, null)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, indexOf, split, take)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import GitHub (Config, Ref(..), fetchPipeline)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Pipeline (RunView, buildPipeline)
import View (renderPipeline)
import Web.HTML (window)
import Web.HTML.Location (search)
import Web.HTML.Window (location)
import Web.HTML.Window as Window

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI rootComponent unit body

type State =
  { config :: Maybe Config
  , pipeline :: Array RunView
  , error :: Maybe String
  , loading :: Boolean
  , formUrl :: String
  , formToken :: String
  }

data Action
  = Initialize
  | Tick
  | OpenUrl String
  | SetFormUrl String
  | SetFormToken String
  | Submit

rootComponent
  :: forall q i o. H.Component q i o Aff
rootComponent =
  H.mkComponent
    { initialState: \_ ->
        { config: Nothing
        , pipeline: []
        , error: Nothing
        , loading: false
        , formUrl: ""
        , formToken: ""
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: State -> H.ComponentHTML Action () Aff
render state = case state.config of
  Nothing -> renderForm state
  Just _ ->
    if state.loading && null state.pipeline then
      HH.div_ [ HH.text "Loading..." ]
    else
      HH.div_
        ( case state.error of
            Just err ->
              [ HH.div
                  [ HP.class_ (HH.ClassName "error") ]
                  [ HH.text err ]
              ]
            Nothing -> []
            <>
              if null state.pipeline then
                [ HH.div
                    [ HP.class_ (HH.ClassName "muted") ]
                    [ HH.text "No workflow runs found." ]
                ]
              else
                [ renderPipeline OpenUrl state.pipeline ]
        )

renderForm
  :: forall w. State -> HH.HTML w Action
renderForm state =
  HH.div
    [ HP.class_ (HH.ClassName "form-container") ]
    [ HH.h1_ [ HH.text "GHA Live" ]
    , HH.p
        [ HP.class_ (HH.ClassName "muted") ]
        [ HH.text "Paste a GitHub URL and token to watch CI live."
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "form") ]
        [ HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder
                "https://github.com/owner/repo/pull/123"
            , HP.value state.formUrl
            , HE.onValueInput SetFormUrl
            , HP.class_ (HH.ClassName "input")
            ]
        , HH.input
            [ HP.type_ HP.InputPassword
            , HP.placeholder "GitHub token"
            , HP.value state.formToken
            , HE.onValueInput SetFormToken
            , HP.class_ (HH.ClassName "input")
            ]
        , HH.button
            [ HE.onClick \_ -> Submit
            , HP.class_ (HH.ClassName "btn")
            ]
            [ HH.text "Watch" ]
        ]
    , case state.error of
        Just err ->
          HH.div
            [ HP.class_ (HH.ClassName "error") ]
            [ HH.text err ]
        Nothing -> HH.text ""
    , HH.p
        [ HP.class_ (HH.ClassName "hint") ]
        [ HH.text
            "Accepted URLs: .../pull/N, .../tree/branch, .../commit/sha"
        ]
    ]

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    qs <- liftEffect do
      w <- window
      loc <- location w
      search loc
    case parseConfig qs of
      Nothing -> pure unit
      Just cfg -> do
        H.modify_ _ { config = Just cfg, loading = true }
        doFetch cfg
        _ <- H.subscribe $ ticker 5000.0
        pure unit
  SetFormUrl url ->
    H.modify_ _ { formUrl = url }
  SetFormToken token ->
    H.modify_ _ { formToken = token }
  Submit -> do
    st <- H.get
    case parseGitHubUrl st.formUrl of
      Nothing ->
        H.modify_ _ { error = Just "Invalid GitHub URL" }
      Just { owner, repo, ref } ->
        let
          cfg =
            { owner, repo, token: st.formToken, ref }
        in
          do
            H.modify_ _
              { config = Just cfg
              , loading = true
              , error = Nothing
              }
            doFetch cfg
            _ <- H.subscribe $ ticker 5000.0
            pure unit
  Tick -> do
    st <- H.get
    case st.config of
      Nothing -> pure unit
      Just cfg -> doFetch cfg
  OpenUrl url -> liftEffect do
    w <- window
    void $ Window.open url "_blank" "" w

doFetch
  :: forall o
   . Config
  -> H.HalogenM State Action () o Aff Unit
doFetch cfg = do
  result <- H.liftAff (fetchPipeline cfg)
  case result of
    Left err ->
      H.modify_ _
        { error = Just err, loading = false }
    Right { runs, jobs } ->
      H.modify_ _
        { pipeline = buildPipeline runs jobs
        , error = Nothing
        , loading = false
        }

ticker :: Number -> HS.Emitter Action
ticker ms = HS.makeEmitter \emit -> do
  fiber <- Aff.launchAff do
    loop emit
  pure $ Aff.launchAff_
    (Aff.killFiber (Aff.error "unsubscribe") fiber)
  where
  loop emit = do
    delay (Milliseconds ms)
    liftEffect (emit Tick)
    loop emit

-- | Parse a GitHub URL into owner, repo, ref.
-- | Supports:
-- |   https://github.com/owner/repo/pull/123
-- |   https://github.com/owner/repo/tree/branch
-- |   https://github.com/owner/repo/commit/sha
-- |   https://github.com/owner/repo  (defaults to Branch "main")
parseGitHubUrl
  :: String
  -> Maybe { owner :: String, repo :: String, ref :: Ref }
parseGitHubUrl url =
  let
    stripped = stripPrefix' "https://github.com/" url
  in
    case stripped of
      Nothing -> Nothing
      Just path ->
        let
          segs = filter (_ /= "")
            $ split (Pattern "/") path
        in
          case index segs 0, index segs 1 of
            Just owner, Just repo -> do
              let ref = parseRefFromSegs segs
              Just { owner, repo, ref }
            _, _ -> Nothing

parseRefFromSegs :: Array String -> Ref
parseRefFromSegs segs =
  case index segs 2, index segs 3 of
    Just "pull", Just n ->
      case Int.fromString n of
        Just i -> PR i
        Nothing -> Branch "main"
    Just "tree", Just b -> Branch b
    Just "commit", Just s -> SHA s
    _, _ -> Branch "main"

stripPrefix' :: String -> String -> Maybe String
stripPrefix' prefix s =
  case indexOf (Pattern prefix) s of
    Just 0 -> Just (drop (prefixLength prefix) s)
    _ -> Nothing

prefixLength :: String -> Int
prefixLength = SCU.length

parseConfig :: String -> Maybe Config
parseConfig qs = do
  let params = parseParams qs
  owner <- lookup' "owner" params
  repo <- lookup' "repo" params
  token <- lookup' "token" params
  ref <- parseRef params
  pure { owner, repo, token, ref }

parseRef
  :: Array { key :: String, value :: String }
  -> Maybe Ref
parseRef params =
  case lookup' "pr" params of
    Just pr -> case Int.fromString pr of
      Just n -> Just (PR n)
      Nothing -> Nothing
    Nothing -> case lookup' "sha" params of
      Just sha -> Just (SHA sha)
      Nothing -> case lookup' "branch" params of
        Just b -> Just (Branch b)
        Nothing -> Nothing

parseParams
  :: String
  -> Array { key :: String, value :: String }
parseParams qs =
  let
    stripped = case indexOf (Pattern "?") qs of
      Just i -> drop (i + 1) qs
      Nothing -> qs
    pairs = filter (\s -> s /= "") $
      split (Pattern "&") stripped
  in
    map parsePair pairs

parsePair
  :: String -> { key :: String, value :: String }
parsePair s =
  case indexOf (Pattern "=") s of
    Just i ->
      { key: take i s
      , value: drop (i + 1) s
      }
    Nothing ->
      { key: s, value: "" }

lookup'
  :: String
  -> Array { key :: String, value :: String }
  -> Maybe String
lookup' k params =
  case filter (\p -> p.key == k) params of
    [ p ] -> Just p.value
    _ -> Nothing
