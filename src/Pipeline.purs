-- | Pipeline model — build the DAG of workflows and jobs from API data.
module Pipeline
  ( Status(..)
  , StepView
  , JobView
  , RunView
  , buildPipeline
  ) where

import Prelude

import Data.Array (filter, sortBy)
import Data.Maybe (Maybe(..))
import GitHub (Step, WorkflowRun, Job)

data Status
  = Pending
  | Queued
  | Running
  | Success
  | Failure
  | Cancelled
  | Skipped

type StepView =
  { name :: String
  , status :: Status
  }

type JobView =
  { id :: Number
  , name :: String
  , status :: Status
  , htmlUrl :: String
  , startedAt :: Maybe String
  , completedAt :: Maybe String
  , steps :: Array StepView
  }

type RunView =
  { id :: Number
  , name :: String
  , status :: Status
  , jobs :: Array JobView
  }

toStatus :: String -> Maybe String -> Status
toStatus status conclusion = case status, conclusion of
  "queued", _ -> Queued
  "in_progress", _ -> Running
  "waiting", _ -> Pending
  "pending", _ -> Pending
  "completed", Just "success" -> Success
  "completed", Just "failure" -> Failure
  "completed", Just "cancelled" -> Cancelled
  "completed", Just "skipped" -> Skipped
  "completed", _ -> Failure
  _, _ -> Pending

buildPipeline
  :: Array WorkflowRun -> Array Job -> Array RunView
buildPipeline runs jobs =
  sortBy (\a b -> compare a.name b.name)
    $ map buildRun runs
  where
  buildRun :: WorkflowRun -> RunView
  buildRun run =
    { id: run.id
    , name: run.name
    , status: toStatus run.status run.conclusion
    , jobs: sortBy (\a b -> compare a.name b.name)
        $ map toJobView (jobsForRun run.id)
    }

  jobsForRun :: Number -> Array Job
  jobsForRun runId = filter (\j -> j.runId == runId) jobs

  toJobView :: Job -> JobView
  toJobView job =
    { id: job.id
    , name: job.name
    , status: toStatus job.status job.conclusion
    , htmlUrl: job.htmlUrl
    , startedAt: job.startedAt
    , completedAt: job.completedAt
    , steps: map toStepView job.steps
    }

  toStepView :: Step -> StepView
  toStepView step =
    { name: step.name
    , status: toStatus step.status step.conclusion
    }
