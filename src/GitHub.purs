-- | GitHub REST API client — fetch workflow runs, jobs, and statuses.
module GitHub
  ( Config
  , Ref(..)
  , WorkflowRun
  , Job
  , fetchPipeline
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concatMap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff, try)
import Effect.Exception (message)
import Fetch (fetch)
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

ghFetch
  :: Config
  -> String
  -> Aff (Either String Json)
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
          }
      }
    resp.text
  case result of
    Left err -> pure $ Left (message err)
    Right body -> case jsonParser body of
      Left e -> pure $ Left ("JSON parse error: " <> e)
      Right json -> pure $ Right json

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
  :: Config -> Aff (Either String (Array WorkflowRun))
fetchRuns cfg = do
  sha <- resolveRef cfg
  case sha of
    Left err -> pure (Left err)
    Right headSha -> do
      result <- ghFetch cfg
        ( "/actions/runs?head_sha=" <> headSha
            <> "&per_page=100"
        )
      pure $ result >>= \json -> do
        runs :: Array WRun <- decodeField "workflow_runs" json
        pure $ map (\(WRun r) -> r) runs

resolveRef :: Config -> Aff (Either String String)
resolveRef cfg = case cfg.ref of
  SHA s -> pure (Right s)
  Branch b -> do
    result <- ghFetch cfg ("/commits/" <> b)
    pure $ result >>= \json -> decodeField "sha" json
  PR n -> do
    result <- ghFetch cfg ("/pulls/" <> show n)
    pure $ result >>= \json -> do
      head :: { sha :: String } <- decodeField "head" json
      pure head.sha

fetchJobs
  :: Config -> Number -> Aff (Either String (Array Job))
fetchJobs cfg runId = do
  result <- ghFetch cfg
    ("/actions/runs/" <> showId runId <> "/jobs?per_page=100")
  pure $ result >>= \json -> do
    jobs :: Array WJob <- decodeField "jobs" json
    pure $ map (\(WJob j) -> j) jobs

fetchPipeline
  :: Config
  -> Aff
       ( Either String
           { runs :: Array WorkflowRun
           , jobs :: Array Job
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
              Right js -> js
          )
          jobResults
      pure $ Right { runs, jobs: allJobs }

showId :: Number -> String
showId = NF.toString
