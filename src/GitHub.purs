-- | GitHub REST API client — fetch workflow runs, jobs, and statuses.
module GitHub
  ( Config
  , OpenPR
  , Ref(..)
  , RateLimit
  , WorkflowRun
  , Job
  , fetchPipeline
  , fetchOpenPRs
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concatMap, index, length)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff, try)
import Effect.Exception (message)
import Fetch (fetch)
import Fetch.Internal.Headers as Headers
import Data.Number.Format as NF
import Foreign.Object as FO

type Config =
  { owner :: String
  , repo :: String
  , token :: String
  , ref :: Ref
  }

data Ref
  = PR Int
  | SHA String
  | Branch String

type WorkflowRun =
  { id :: Number
  , name :: String
  , status :: String
  , conclusion :: Maybe String
  , htmlUrl :: String
  , headSha :: String
  }

type Job =
  { id :: Number
  , name :: String
  , status :: String
  , conclusion :: Maybe String
  , htmlUrl :: String
  , runId :: Number
  }

newtype WRun = WRun WorkflowRun

instance DecodeJson WRun where
  decodeJson json = case toObject json of
    Nothing -> Left (TypeMismatch "Object")
    Just obj -> do
      id_ <- obj .: "id"
      name_ <- obj .: "name"
      status_ <- obj .: "status"
      conclusion_ <- obj .:? "conclusion"
      htmlUrl_ <- obj .: "html_url"
      headSha_ <- obj .: "head_sha"
      pure $ WRun
        { id: id_
        , name: name_
        , status: status_
        , conclusion: conclusion_
        , htmlUrl: htmlUrl_
        , headSha: headSha_
        }

newtype WJob = WJob Job

instance DecodeJson WJob where
  decodeJson json = case toObject json of
    Nothing -> Left (TypeMismatch "Object")
    Just obj -> do
      id_ <- obj .: "id"
      name_ <- obj .: "name"
      status_ <- obj .: "status"
      conclusion_ <- obj .:? "conclusion"
      htmlUrl_ <- obj .: "html_url"
      runId_ <- obj .: "run_id"
      pure $ WJob
        { id: id_
        , name: name_
        , status: status_
        , conclusion: conclusion_
        , htmlUrl: htmlUrl_
        , runId: runId_
        }

type OpenPR =
  { number :: Int
  , title :: String
  }

newtype WPR = WPR OpenPR

instance DecodeJson WPR where
  decodeJson json = case toObject json of
    Nothing -> Left (TypeMismatch "Object")
    Just obj -> do
      number_ <- obj .: "number"
      title_ <- obj .: "title"
      pure $ WPR { number: number_, title: title_ }

type RateLimit =
  { remaining :: Int
  , limit :: Int
  }

type GHResponse =
  { json :: Json
  , rateLimit :: Maybe RateLimit
  }

ghFetch
  :: Config
  -> String
  -> Aff (Either String GHResponse)
ghFetch cfg path = do
  let
    url =
      "https://api.github.com/repos/"
        <> cfg.owner
        <> "/"
        <> cfg.repo
        <> path
  result <- try do
    resp <- fetch url
      { headers:
          { "Accept": "application/vnd.github.v3+json"
          , "Authorization": "Bearer " <> cfg.token
          , "If-None-Match": ""
          }
      }
    body <- resp.text
    pure { body, headers: resp.headers }
  case result of
    Left err -> pure $ Left (message err)
    Right r -> case jsonParser r.body of
      Left e -> pure $ Left ("JSON parse error: " <> e)
      Right json ->
        let
          rl = do
            rem <-
              Headers.lookup "x-ratelimit-remaining"
                r.headers
                >>= Int.fromString
            lim <-
              Headers.lookup "x-ratelimit-limit"
                r.headers
                >>= Int.fromString
            Just { remaining: rem, limit: lim }
        in
          pure $ Right { json, rateLimit: rl }

decodeField
  :: forall a
   . DecodeJson a
  => String
  -> Json
  -> Either String a
decodeField field json = case toObject json of
  Nothing -> Left "Expected JSON object"
  Just obj -> case FO.lookup field obj of
    Nothing -> Left ("Missing field: " <> field)
    Just arr -> case decodeJson arr of
      Left err -> Left (show err)
      Right val -> pure val

fetchRuns
  :: Config
  -> Aff (Either String (Array WorkflowRun))
fetchRuns cfg = do
  sha <- resolveRef cfg
  case sha of
    Left err -> pure (Left err)
    Right headSha -> do
      result <- ghFetch cfg
        ( "/actions/runs?head_sha=" <> headSha
            <> "&per_page=100"
        )
      pure $ result >>= \r -> do
        runs :: Array WRun <- decodeField
          "workflow_runs"
          r.json
        pure $ map (\(WRun r') -> r') runs

resolveRef :: Config -> Aff (Either String String)
resolveRef cfg = case cfg.ref of
  SHA s -> pure (Right s)
  Branch b -> do
    result <- ghFetch cfg ("/commits/" <> b)
    pure $ result >>= \r -> decodeField "sha" r.json
  PR n -> do
    result <- ghFetch cfg ("/pulls/" <> show n)
    pure $ result >>= \r -> do
      head :: { sha :: String } <- decodeField "head"
        r.json
      pure head.sha

fetchJobs
  :: Config
  -> Number
  -> Aff
       ( Either String
           { jobs :: Array Job
           , rateLimit :: Maybe RateLimit
           }
       )
fetchJobs cfg runId = do
  result <- ghFetch cfg
    ( "/actions/runs/" <> showId runId
        <> "/jobs?per_page=100"
    )
  pure $ result >>= \r -> do
    jobs :: Array WJob <- decodeField "jobs" r.json
    pure
      { jobs: map (\(WJob j) -> j) jobs
      , rateLimit: r.rateLimit
      }

fetchPipeline
  :: Config
  -> Aff
       ( Either String
           { runs :: Array WorkflowRun
           , jobs :: Array Job
           , rateLimit :: Maybe RateLimit
           }
       )
fetchPipeline cfg = do
  runsResult <- fetchRuns cfg
  case runsResult of
    Left err -> pure (Left err)
    Right runs -> do
      jobResults <- traverse
        (\r -> fetchJobs cfg r.id)
        runs
      let
        allJobs = concatMap
          ( case _ of
              Left _ -> []
              Right r -> r.jobs
          )
          jobResults
        lastRL = fromMaybe Nothing $ map _.rateLimit
          $
            concatMap
              ( case _ of
                  Left _ -> []
                  Right r -> [ r ]
              )
              jobResults
              # lastElem
      pure $ Right
        { runs, jobs: allJobs, rateLimit: lastRL }

lastElem :: forall a. Array a -> Maybe a
lastElem arr = index arr (length arr - 1)

fetchOpenPRs
  :: Config
  -> Aff
       ( Either String
           { prs :: Array OpenPR
           , rateLimit :: Maybe RateLimit
           }
       )
fetchOpenPRs cfg = do
  result <- ghFetch cfg "/pulls?state=open&per_page=100"
  pure $ result >>= \r -> do
    prs :: Array WPR <- decodeJson r.json
      # lmap show
    pure
      { prs: map (\(WPR p) -> p) prs
      , rateLimit: r.rateLimit
      }
  where
  lmap f (Left e) = Left (f e)
  lmap _ (Right a) = Right a

showId :: Number -> String
showId = NF.toString
