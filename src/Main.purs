module Main where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, foldl, index, length, nub, null, snoc)
import Data.Either (Either(..), hush)
import Data.Int (ceil, fromString, toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), drop, indexOf, split, take)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import GitHub
  ( Config
  , RateLimit
  , Ref(..)
  , fetchOpenPRs
  , fetchPipeline
  )
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
  , interval :: Int
  , secondsLeft :: Int
  , apiCalls :: Int
  , rateLimit :: Maybe RateLimit
  }

data Action
  = Initialize
  | Tick
  | SetFormUrl String
  | SetFormToken String
  | Submit
  | Back
  | Refresh
  | SelectTarget Target
  | RemoveTarget Target
  | ChangeInterval Int

storageKeyTargets :: String
storageKeyTargets = "gha-live-targets"

storageKeyToken :: String
storageKeyToken = "gha-live-token"

storageKeyUrl :: String
storageKeyUrl = "gha-live-url"

storageKeyConfig :: String
storageKeyConfig = "gha-live-config"

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
        , interval: 5
        , secondsLeft: 5
        , apiCalls: 0
        , rateLimit: Nothing
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
    HH.div
      [ HP.class_ (HH.ClassName "layout") ]
      [ renderSidebar state
      , HH.div
          [ HP.class_ (HH.ClassName "main") ]
          ( [ renderToolbar state ]
              <>
                if state.loading && null state.pipeline then
                  [ HH.text "Loading..." ]
                else
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
                              [ HP.class_
                                  (HH.ClassName "muted")
                              ]
                              [ HH.text
                                  "No workflow runs found."
                              ]
                          ]
                        else
                          [ renderPipeline state.pipeline
                          ]
                  )
          )
      ]

type TargetParts =
  { owner :: String
  , repo :: String
  , ref :: String
  , target :: Target
  }

parseTargetLabel :: Target -> TargetParts
parseTargetLabel target =
  case indexOf (Pattern "/") target.label of
    Nothing ->
      { owner: target.label
      , repo: ""
      , ref: ""
      , target
      }
    Just slashIdx ->
      let
        owner = take slashIdx target.label
        rest = drop (slashIdx + 1) target.label
      in
        case indexOf (Pattern " ") rest of
          Nothing ->
            { owner, repo: rest, ref: "", target }
          Just spaceIdx ->
            { owner
            , repo: take spaceIdx rest
            , ref: drop spaceIdx rest
            , target
            }

type RepoNode =
  { repo :: String
  , entries :: Array TargetParts
  }

type OwnerNode =
  { owner :: String
  , repos :: Array RepoNode
  }

buildTree :: Array Target -> Array OwnerNode
buildTree targets =
  let
    parts = map parseTargetLabel targets
    owners = nub (map _.owner parts)
  in
    map
      ( \o ->
          let
            ownerParts = filter (_.owner >>> eq o) parts
            repos = nub (map _.repo ownerParts)
          in
            { owner: o
            , repos: map
                ( \r ->
                    { repo: r
                    , entries: filter
                        (_.repo >>> eq r)
                        ownerParts
                    }
                )
                repos
            }
      )
      owners

renderSidebar
  :: forall w. State -> HH.HTML w Action
renderSidebar state =
  HH.div
    [ HP.class_ (HH.ClassName "sidebar") ]
    ( [ HH.div
          [ HP.class_ (HH.ClassName "sidebar-title") ]
          [ HH.text "Targets" ]
      ]
        <> bind (buildTree state.targets)
          renderOwnerNode
    )

renderOwnerNode
  :: forall w. OwnerNode -> Array (HH.HTML w Action)
renderOwnerNode node =
  [ HH.div
      [ HP.class_ (HH.ClassName "tree-owner") ]
      [ HH.text node.owner ]
  ]
    <> bind node.repos renderRepoNode

renderRepoNode
  :: forall w. RepoNode -> Array (HH.HTML w Action)
renderRepoNode node =
  [ HH.div
      [ HP.class_ (HH.ClassName "tree-repo") ]
      [ HH.text node.repo ]
  ]
    <> map renderRefItem node.entries

renderRefItem
  :: forall w. TargetParts -> HH.HTML w Action
renderRefItem parts =
  HH.div
    [ HE.onClick \_ -> SelectTarget parts.target
    , HP.class_ (HH.ClassName "tree-ref")
    ]
    [ HH.text
        if parts.ref == "" then "(default)"
        else parts.ref
    ]

renderToolbar
  :: forall w. State -> HH.HTML w Action
renderToolbar state =
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
        , HP.disabled state.loading
        ]
        [ HH.text
            if state.loading then "Refreshing..."
            else "Refresh"
        ]
    , HH.span
        [ HP.class_ (HH.ClassName "toolbar-timer") ]
        [ HH.button
            [ HE.onClick \_ -> ChangeInterval (-5)
            , HP.class_ (HH.ClassName "btn-small")
            , HP.disabled (state.interval <= 5)
            ]
            [ HH.text "-" ]
        , HH.text
            ( show state.secondsLeft <> "s / "
                <> show state.interval
                <> "s"
            )
        , HH.button
            [ HE.onClick \_ -> ChangeInterval 5
            , HP.class_ (HH.ClassName "btn-small")
            ]
            [ HH.text "+" ]
        , HH.text
            ( " · " <> show state.apiCalls
                <> " calls"
            )
        , case state.rateLimit of
            Nothing -> HH.text ""
            Just rl ->
              HH.text
                ( " · " <> show rl.remaining
                    <> "/"
                    <> show rl.limit
                )
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
              <> bind (buildTree targets)
                renderFormOwner
          )
      ]

renderFormOwner
  :: forall w. OwnerNode -> Array (HH.HTML w Action)
renderFormOwner node =
  [ HH.div
      [ HP.class_ (HH.ClassName "tree-owner form-tree-owner") ]
      [ HH.text node.owner ]
  ]
    <> bind node.repos renderFormRepo

renderFormRepo
  :: forall w. RepoNode -> Array (HH.HTML w Action)
renderFormRepo node =
  [ HH.div
      [ HP.class_ (HH.ClassName "tree-repo form-tree-repo") ]
      [ HH.text node.repo ]
  ]
    <> map renderFormRef node.entries

renderFormRef
  :: forall w. TargetParts -> HH.HTML w Action
renderFormRef parts =
  HH.div
    [ HP.class_ (HH.ClassName "target") ]
    [ HH.span
        [ HE.onClick \_ -> SelectTarget parts.target
        , HP.class_ (HH.ClassName "target-label")
        ]
        [ HH.text
            if parts.ref == "" then "(default)"
            else parts.ref
        ]
    , HH.span
        [ HE.onClick \_ -> RemoveTarget parts.target
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
      Just cfg -> startWatching cfg
      Nothing -> case saved.config of
        Just cfg -> startWatching cfg
        Nothing -> pure unit
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
        H.modify_ _
          { loading = true
          , secondsLeft = st.interval
          }
        doFetch cfg
  Back -> do
    H.modify_ _
      { config = Nothing
      , pipeline = []
      , error = Nothing
      , loading = false
      }
    liftEffect clearConfig
  ChangeInterval delta -> do
    st <- H.get
    let
      newInterval = max 5 (st.interval + delta)
    H.modify_ _
      { interval = newInterval
      , secondsLeft = min st.secondsLeft newInterval
      }
  Tick -> do
    st <- H.get
    case st.config of
      Nothing -> pure unit
      Just cfg ->
        if st.secondsLeft <= 1 then do
          H.modify_ _ { loading = true, secondsLeft = st.interval }
          doFetch cfg
        else
          H.modify_ _ { secondsLeft = st.secondsLeft - 1 }

startWatching
  :: forall o
   . Config
  -> H.HalogenM State Action () o Aff Unit
startWatching cfg = do
  st <- H.get
  H.modify_ _
    { config = Just cfg
    , loading = true
    , error = Nothing
    , secondsLeft = st.interval
    }
  liftEffect $ saveConfig cfg
  doFetch cfg
  _ <- H.subscribe $ ticker 1000.0
  pure unit

doFetch
  :: forall o
   . Config
  -> H.HalogenM State Action () o Aff Unit
doFetch cfg = do
  result <- H.liftAff (fetchPipeline cfg)
  prsResult <- H.liftAff (fetchOpenPRs cfg)
  let
    refCalls = case cfg.ref of
      SHA _ -> 0
      _ -> 1
  case result of
    Left err ->
      H.modify_ _
        { error = Just err, loading = false }
    Right { runs, jobs, rateLimit } ->
      let
        prCalls = 1
        calls = refCalls + 1 + length runs + prCalls
        rl' = case prsResult of
          Right { rateLimit: Just r } -> Just r
          _ -> rateLimit
        optInterval = case rl' of
          Nothing -> Nothing
          Just rl ->
            let
              secs = Int.ceil
                ( 7200.0 * Int.toNumber calls
                    / Int.toNumber rl.limit
                )
            in
              Just (max 5 secs)
        prTargets = case prsResult of
          Left _ -> []
          Right { prs } -> map
            ( \pr ->
                { url: "https://github.com/"
                    <> cfg.owner
                    <> "/"
                    <> cfg.repo
                    <> "/pull/"
                    <> show pr.number
                , label: cfg.owner <> "/" <> cfg.repo
                    <> " #"
                    <> show pr.number
                }
            )
            prs
      in
        do
          st <- H.get
          let
            merged = foldl (flip addTarget) st.targets
              prTargets
            newInterval = fromMaybe st.interval
              optInterval
          H.modify_ _
            { pipeline = buildPipeline runs jobs
            , error = Nothing
            , loading = false
            , apiCalls = calls
            , rateLimit = rl'
            , interval = newInterval
            , secondsLeft = newInterval
            , targets = merged
            }
          liftEffect $ saveTargets merged

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
  , config :: Maybe Config
  }

loadSaved :: Effect Saved
loadSaved = do
  w <- window
  s <- localStorage w
  url <- fromMaybe "" <$> Storage.getItem storageKeyUrl s
  token <- fromMaybe "" <$>
    Storage.getItem storageKeyToken s
  targets <- loadTargets s
  config <- loadConfig s
  pure { url, token, targets, config }

loadConfig :: Storage.Storage -> Effect (Maybe Config)
loadConfig s = do
  raw <- Storage.getItem storageKeyConfig s
  pure $ case raw of
    Nothing -> Nothing
    Just str -> do
      json <- hush (jsonParser str)
      obj <-
        hush (decodeJson json)
          :: Maybe
               { owner :: String
               , repo :: String
               , token :: String
               , refTag :: String
               , refVal :: String
               }
      ref <- case obj.refTag of
        "pr" -> PR <$> Int.fromString obj.refVal
        "sha" -> Just (SHA obj.refVal)
        "branch" -> Just (Branch obj.refVal)
        _ -> Nothing
      Just
        { owner: obj.owner
        , repo: obj.repo
        , token: obj.token
        , ref
        }

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

saveConfig :: Config -> Effect Unit
saveConfig cfg = do
  let
    { tag, val } = case cfg.ref of
      PR n -> { tag: "pr", val: show n }
      SHA s -> { tag: "sha", val: s }
      Branch b -> { tag: "branch", val: b }
    json = encodeJson
      { owner: cfg.owner
      , repo: cfg.repo
      , token: cfg.token
      , refTag: tag
      , refVal: val
      }
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyConfig (stringify json) s

clearConfig :: Effect Unit
clearConfig = do
  w <- window
  s <- localStorage w
  Storage.removeItem storageKeyConfig s

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
