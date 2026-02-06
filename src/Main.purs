module Main where

import Prelude

import Data.Array (filter, null)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, indexOf, split, take)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import GitHub (Config, Ref(..), fetchPipeline)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
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
  }

data Action
  = Initialize
  | Tick
  | OpenUrl String

rootComponent
  :: forall q i o. H.Component q i o Aff
rootComponent =
  H.mkComponent
    { initialState: \_ ->
        { config: Nothing
        , pipeline: []
        , error: Nothing
        , loading: false
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: State -> H.ComponentHTML Action () Aff
render state
  | state.loading && null state.pipeline =
      HH.div_ [ HH.text "Loading..." ]
  | otherwise =
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
                [ HH.div_ [ HH.text "No workflow runs found." ]
                ]
              else
                [ renderPipeline OpenUrl state.pipeline ]
        )

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
      Nothing ->
        H.modify_ _
          { error = Just
              "Missing query params. Use ?owner=X&repo=Y&token=Z&branch=main (or &pr=N or &sha=ABC)"
          }
      Just cfg -> do
        H.modify_ _ { config = Just cfg, loading = true }
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
  pure $ Aff.launchAff_ (Aff.killFiber (Aff.error "unsubscribe") fiber)
  where
  loop emit = do
    delay (Milliseconds ms)
    liftEffect (emit Tick)
    loop emit

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
    pairs = filter (\s -> s /= "") $ split (Pattern "&") stripped
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
