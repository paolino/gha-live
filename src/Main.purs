module Main where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (any, filter, find, foldl, index, length, nub, null, snoc)
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
  , title :: String
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
  , tickSub :: Maybe H.SubscriptionId
  , showSidebarForm :: Boolean
  , showTokenForm :: Boolean
  , headingRepo :: String
  , headingTitle :: String
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
  | ToggleSidebarForm
  | ChangeInterval Int
  | RemoveRepo String String
  | ToggleTokenForm

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
        , tickSub: Nothing
        , showSidebarForm: false
        , showTokenForm: false
        , headingRepo: ""
        , headingTitle: ""
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
          ( [ renderToolbar state
            , if state.headingRepo == "" then HH.text ""
              else
                HH.h2
                  [ HP.class_ (HH.ClassName "heading") ]
                  [ HH.span
                      [ HP.class_
                          (HH.ClassName "heading-repo")
                      ]
                      [ HH.text
                          (state.headingRepo <> " — ")
                      ]
                  , HH.span
                      [ HP.class_
                          (HH.ClassName "heading-title")
                      ]
                      [ HH.text state.headingTitle ]
                  ]
            ]
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

renderInputs
  :: forall w. State -> Array (HH.HTML w Action)
renderInputs state =
  ( if state.showSidebarForm then
      [ HH.div
          [ HP.class_ (HH.ClassName "sidebar-form") ]
          [ HH.div
              [ HP.class_
                  (HH.ClassName "sidebar-form-row")
              ]
              [ HH.input
                  [ HP.type_ HP.InputText
                  , HP.placeholder "GitHub URL"
                  , HP.value state.formUrl
                  , HE.onValueInput SetFormUrl
                  , HP.class_
                      ( HH.ClassName
                          "input input-sm url-input"
                      )
                  ]
              , HH.button
                  [ HE.onClick \_ -> Submit
                  , HP.class_ (HH.ClassName "btn btn-sm")
                  ]
                  [ HH.text "Watch" ]
              , HH.button
                  [ HE.onClick \_ -> ToggleSidebarForm
                  , HP.class_ (HH.ClassName "btn-small")
                  ]
                  [ HH.text "x" ]
              ]
          ]
      ]
    else
      [ HH.button
          [ HE.onClick \_ -> ToggleSidebarForm
          , HP.class_ (HH.ClassName "btn-add")
          ]
          [ HH.text "+" ]
      ]
  )
    <>
      if state.showTokenForm then
        [ HH.div
            [ HP.class_ (HH.ClassName "sidebar-form") ]
            [ HH.div
                [ HP.class_
                    (HH.ClassName "sidebar-form-row")
                ]
                [ HH.input
                    [ HP.type_ HP.InputPassword
                    , HP.placeholder "Token"
                    , HP.value state.formToken
                    , HE.onValueInput SetFormToken
                    , HP.class_
                        ( HH.ClassName
                            "input input-sm url-input"
                        )
                    ]
                , HH.button
                    [ HE.onClick \_ -> ToggleTokenForm
                    , HP.class_ (HH.ClassName "btn-small")
                    ]
                    [ HH.text "x" ]
                ]
            ]
        ]
      else
        [ HH.button
            [ HE.onClick \_ -> ToggleTokenForm
            , HP.class_ (HH.ClassName "btn-add btn-add-token")
            ]
            [ HH.text "\x1F511" ]
        ]

renderSidebar
  :: forall w. State -> HH.HTML w Action
renderSidebar state =
  HH.div
    [ HP.class_ (HH.ClassName "sidebar") ]
    ( renderInputs state
        <>
          [ HH.div
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
    <> bind node.repos (renderRepoNode node.owner)

renderRepoNode
  :: forall w
   . String
  -> RepoNode
  -> Array (HH.HTML w Action)
renderRepoNode owner node =
  [ HH.div
      [ HP.class_ (HH.ClassName "tree-repo") ]
      [ HH.span_ [ HH.text node.repo ]
      , HH.span
          [ HE.onClick \_ -> RemoveRepo owner node.repo
          , HP.class_ (HH.ClassName "tree-remove")
          ]
          [ HH.text "x" ]
      ]
  ]
    <> map renderRefItem node.entries

truncate :: Int -> String -> String
truncate n s =
  if SCU.length s <= n then s
  else take n s <> "…"

renderRefItem
  :: forall w. TargetParts -> HH.HTML w Action
renderRefItem parts =
  HH.div
    [ HE.onClick \_ -> SelectTarget parts.target
    , HP.class_ (HH.ClassName "tree-ref")
    , HP.title (refText <> titleText)
    ]
    [ HH.text refText
    , if parts.target.title == "" then HH.text ""
      else
        HH.span
          [ HP.class_ (HH.ClassName "tree-ref-title") ]
          [ HH.text
              (" " <> truncate 44 parts.target.title)
          ]
    ]
  where
  refText =
    if parts.ref == "" then "(default)"
    else parts.ref
  titleText =
    if parts.target.title == "" then ""
    else " " <> parts.target.title

renderToolbar
  :: forall w. State -> HH.HTML w Action
renderToolbar state =
  HH.div
    [ HP.class_ (HH.ClassName "toolbar") ]
    [ HH.button
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
              "Watch GitHub Actions pipelines in real time."
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
      , HH.div
          [ HP.class_ (HH.ClassName "instructions") ]
          [ HH.h3_ [ HH.text "Getting started" ]
          , HH.ol_
              [ HH.li_
                  [ HH.text "Create a "
                  , HH.strong_ [ HH.text "GitHub token" ]
                  , HH.text
                      " with read access to Actions and Pull Requests"
                  ]
              , HH.li_
                  [ HH.text
                      "Paste any GitHub URL: PR, branch, commit, or repo"
                  ]
              , HH.li_
                  [ HH.text
                      "Click Watch to see workflow runs and jobs live"
                  ]
              ]
          , HH.h3_ [ HH.text "Supported URLs" ]
          , HH.ul
              [ HP.class_ (HH.ClassName "url-examples") ]
              [ HH.li_
                  [ HH.code_
                      [ HH.text
                          ".../owner/repo/pull/123"
                      ]
                  , HH.text " — watch a PR"
                  ]
              , HH.li_
                  [ HH.code_
                      [ HH.text
                          ".../owner/repo/tree/branch"
                      ]
                  , HH.text " — watch a branch"
                  ]
              , HH.li_
                  [ HH.code_
                      [ HH.text
                          ".../owner/repo/commit/sha"
                      ]
                  , HH.text " — watch a commit"
                  ]
              , HH.li_
                  [ HH.code_
                      [ HH.text ".../owner/repo" ]
                  , HH.text " — watch main branch"
                  ]
              ]
          , HH.h3_ [ HH.text "Features" ]
          , HH.ul_
              [ HH.li_
                  [ HH.text
                      "Auto-refresh with adaptive rate limiting"
                  ]
              , HH.li_
                  [ HH.text
                      "Open PRs are discovered automatically"
                  ]
              , HH.li_
                  [ HH.text
                      "Targets are saved in your browser"
                  ]
              , HH.li_
                  [ HH.text
                      "Click any job to open it on GitHub"
                  ]
              ]
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
    <> bind node.repos (renderFormRepo node.owner)

renderFormRepo
  :: forall w
   . String
  -> RepoNode
  -> Array (HH.HTML w Action)
renderFormRepo owner node =
  [ HH.div
      [ HP.class_ (HH.ClassName "tree-repo form-tree-repo") ]
      [ HH.span_ [ HH.text node.repo ]
      , HH.span
          [ HE.onClick \_ -> RemoveRepo owner node.repo
          , HP.class_ (HH.ClassName "tree-remove")
          ]
          [ HH.text "x" ]
      ]
  ]
    <> map renderFormRef node.entries

renderFormRef
  :: forall w. TargetParts -> HH.HTML w Action
renderFormRef parts =
  HH.div
    [ HP.class_ (HH.ClassName "target")
    , HP.title (refText <> titleText)
    ]
    [ HH.span
        [ HE.onClick \_ -> SelectTarget parts.target
        , HP.class_ (HH.ClassName "target-label")
        ]
        ( [ HH.text refText ]
            <>
              if parts.target.title == "" then []
              else
                [ HH.span
                    [ HP.class_
                        (HH.ClassName "target-title")
                    ]
                    [ HH.text
                        ( " " <> truncate 36
                            parts.target.title
                        )
                    ]
                ]
        )
    , HH.span
        [ HE.onClick \_ -> RemoveTarget parts.target
        , HP.class_ (HH.ClassName "target-remove")
        ]
        [ HH.text "x" ]
    ]
  where
  refText =
    if parts.ref == "" then "(default)"
    else parts.ref
  titleText =
    if parts.target.title == "" then ""
    else " " <> parts.target.title

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
            , title: ""
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
  RemoveRepo owner repo -> do
    st <- H.get
    let
      newTargets = filter
        ( \t ->
            let
              p = parseTargetLabel t
            in
              not
                (p.owner == owner && p.repo == repo)
        )
        st.targets
    H.modify_ _ { targets = newTargets }
    liftEffect $ saveTargets newTargets
  ToggleSidebarForm ->
    H.modify_ \st ->
      st { showSidebarForm = not st.showSidebarForm }
  ToggleTokenForm ->
    H.modify_ \st ->
      st { showTokenForm = not st.showTokenForm }
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
    st <- H.get
    case st.tickSub of
      Just sid -> H.unsubscribe sid
      Nothing -> pure unit
    H.modify_ _
      { config = Nothing
      , pipeline = []
      , error = Nothing
      , loading = false
      , tickSub = Nothing
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
  case st.tickSub of
    Just sid -> H.unsubscribe sid
    Nothing -> pure unit
  H.modify_ _
    { config = Just cfg
    , loading = true
    , error = Nothing
    , secondsLeft = st.interval
    }
  liftEffect $ saveConfig cfg
  doFetch cfg
  sid <- H.subscribe $ ticker 1000.0
  H.modify_ _ { tickSub = Just sid }

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
                , title: pr.title
                }
            )
            prs
        title = case cfg.ref of
          PR n ->
            let
              prTitle = case prsResult of
                Left _ -> Nothing
                Right { prs } ->
                  map _.title
                    (find (\p -> p.number == n) prs)
            in
              "#" <> show n <> fromMaybe ""
                (map (\t -> " " <> t) prTitle)
          Branch b -> b
          SHA s -> take 7 s
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
            , targets = merged
            , headingRepo = cfg.owner <> "/" <> cfg.repo
            , headingTitle = title
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
  if any (\x -> x.url == t.url) ts then
    map
      ( \x ->
          if x.url == t.url && t.title /= "" then
            x { title = t.title }
          else x
      )
      ts
  else snoc ts t

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
decodeTargets json =
  case decodeJson json of
    Right
      ( arr
          :: Array
               { url :: String
               , label :: String
               , title :: String
               }
      ) -> Just arr
    _ -> case decodeJson json of
      Right
        ( arr
            :: Array { url :: String, label :: String }
        ) -> Just $ map
        (\t -> { url: t.url, label: t.label, title: "" })
        arr
      _ -> Nothing

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
