module Main where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, index, nub, null, snoc)
import Data.Either (Either(..), hush)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe)
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
import Web.HTML.Window (localStorage, location)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI rootComponent unit body

type Target =
  { url :: String
  , label :: String
  }

type State =
  { config :: Maybe Config
  , pipeline :: Array RunView
  , error :: Maybe String
  , loading :: Boolean
  , formUrl :: String
  , formToken :: String
  , targets :: Array Target
  }

data Action
  = Initialize
  | Tick
  | OpenUrl String
  | SetFormUrl String
  | SetFormToken String
  | Submit
  | Back
  | Refresh
  | SelectTarget Target
  | RemoveTarget Target

storageKeyTargets :: String
storageKeyTargets = "gha-live-targets"

storageKeyToken :: String
storageKeyToken = "gha-live-token"

storageKeyUrl :: String
storageKeyUrl = "gha-live-url"

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
        , targets: []
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
      HH.div_
        [ renderToolbar true
        , HH.text "Loading..."
        ]
    else
      HH.div_
        ( [ renderToolbar state.loading ]
            <> case state.error of
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
                    [ HH.text
                        "No workflow runs found."
                    ]
                ]
              else
                [ renderPipeline OpenUrl state.pipeline
                ]
        )

renderToolbar
  :: forall w. Boolean -> HH.HTML w Action
renderToolbar loading =
  HH.div
    [ HP.class_ (HH.ClassName "toolbar") ]
    [ HH.button
        [ HE.onClick \_ -> Back
        , HP.class_ (HH.ClassName "btn-back")
        ]
        [ HH.text "Back" ]
    , HH.button
        [ HE.onClick \_ -> Refresh
        , HP.class_ (HH.ClassName "btn-back")
        , HP.disabled loading
        ]
        [ HH.text
            if loading then "Refreshing..." else "Refresh"
        ]
    ]

renderForm
  :: forall w. State -> HH.HTML w Action
renderForm state =
  HH.div
    [ HP.class_ (HH.ClassName "form-container") ]
    ( [ HH.h1_ [ HH.text "GHA Live" ]
      , HH.p
          [ HP.class_ (HH.ClassName "muted") ]
          [ HH.text
              "Paste a GitHub URL and token to watch CI live."
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
              "URLs: .../pull/N, .../issues/N, .../tree/branch, .../commit/sha"
          ]
      ]
        <> renderTargets state.targets
    )

renderTargets
  :: forall w. Array Target -> Array (HH.HTML w Action)
renderTargets targets
  | null targets = []
  | otherwise =
      [ HH.div
          [ HP.class_ (HH.ClassName "targets") ]
          ( [ HH.h3_ [ HH.text "Recent" ] ]
              <> map renderTarget targets
          )
      ]

renderTarget
  :: forall w. Target -> HH.HTML w Action
renderTarget target =
  HH.div
    [ HP.class_ (HH.ClassName "target") ]
    [ HH.span
        [ HE.onClick \_ -> SelectTarget target
        , HP.class_ (HH.ClassName "target-label")
        ]
        [ HH.text target.label ]
    , HH.span
        [ HE.onClick \_ -> RemoveTarget target
        , HP.class_ (HH.ClassName "target-remove")
        ]
        [ HH.text "x" ]
    ]

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    saved <- liftEffect loadSaved
    H.modify_ _
      { formUrl = saved.url
      , formToken = saved.token
      , targets = saved.targets
      }
    qs <- liftEffect do
      w <- window
      loc <- location w
      search loc
    case parseConfig qs of
      Nothing -> pure unit
      Just cfg -> startWatching cfg
  SetFormUrl url -> do
    H.modify_ _ { formUrl = url }
    liftEffect $ saveField storageKeyUrl url
  SetFormToken token -> do
    H.modify_ _ { formToken = token }
    liftEffect $ saveField storageKeyToken token
  Submit -> do
    st <- H.get
    case parseGitHubUrl st.formUrl of
      Nothing ->
        H.modify_ _
          { error = Just "Invalid GitHub URL" }
      Just { owner, repo, ref } ->
        let
          cfg =
            { owner
            , repo
            , token: st.formToken
            , ref
            }
          target =
            { url: st.formUrl
            , label: owner <> "/" <> repo <> refLabel ref
            }
        in
          do
            let
              newTargets = addTarget target st.targets
            H.modify_ _ { targets = newTargets }
            liftEffect $ saveTargets newTargets
            startWatching cfg
  SelectTarget target -> do
    H.modify_ _ { formUrl = target.url }
    liftEffect $ saveField storageKeyUrl target.url
    handleAction Submit
  RemoveTarget target -> do
    st <- H.get
    let
      newTargets = filter
        (\t -> t.url /= target.url)
        st.targets
    H.modify_ _ { targets = newTargets }
    liftEffect $ saveTargets newTargets
  Refresh -> do
    st <- H.get
    case st.config of
      Nothing -> pure unit
      Just cfg -> do
        H.modify_ _ { loading = true }
        doFetch cfg
  Back -> do
    H.modify_ _
      { config = Nothing
      , pipeline = []
      , error = Nothing
      , loading = false
      }
  Tick -> do
    st <- H.get
    case st.config of
      Nothing -> pure unit
      Just cfg -> doFetch cfg
  OpenUrl url -> liftEffect do
    w <- window
    void $ Window.open url "_blank" "" w

startWatching
  :: forall o
   . Config
  -> H.HalogenM State Action () o Aff Unit
startWatching cfg = do
  H.modify_ _
    { config = Just cfg
    , loading = true
    , error = Nothing
    }
  doFetch cfg
  _ <- H.subscribe $ ticker 5000.0
  pure unit

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

refLabel :: Ref -> String
refLabel = case _ of
  PR n -> " #" <> show n
  SHA s -> " @" <> take 7 s
  Branch b -> " (" <> b <> ")"

addTarget :: Target -> Array Target -> Array Target
addTarget t ts =
  nub $ snoc (filter (\x -> x.url /= t.url) ts) t

-- localStorage helpers

type Saved =
  { url :: String
  , token :: String
  , targets :: Array Target
  }

loadSaved :: Effect Saved
loadSaved = do
  w <- window
  s <- localStorage w
  url <- fromMaybe "" <$> Storage.getItem storageKeyUrl s
  token <- fromMaybe "" <$>
    Storage.getItem storageKeyToken s
  targets <- loadTargets s
  pure { url, token, targets }

loadTargets :: Storage.Storage -> Effect (Array Target)
loadTargets s = do
  raw <- Storage.getItem storageKeyTargets s
  pure $ case raw of
    Nothing -> []
    Just str -> case hush (jsonParser str) >>= decodeTargets of
      Nothing -> []
      Just ts -> ts

decodeTargets :: Json -> Maybe (Array Target)
decodeTargets json = case decodeJson json of
  Left _ -> Nothing
  Right (arr :: Array { url :: String, label :: String }) ->
    Just arr

saveTargets :: Array Target -> Effect Unit
saveTargets targets = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyTargets
    (stringify (encodeJson targets))
    s

saveField :: String -> String -> Effect Unit
saveField key val = do
  w <- window
  s <- localStorage w
  Storage.setItem key val s

-- GitHub URL parsing

parseGitHubUrl
  :: String
  -> Maybe
       { owner :: String
       , repo :: String
       , ref :: Ref
       }
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
    Just "issues", Just n ->
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

-- Query param parsing (for backward compat)

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
