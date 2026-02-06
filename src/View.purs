-- | View — render the pipeline DAG as SVG with Halogen.
module View
  ( renderPipeline
  ) where

import Prelude

import Data.Array as Array
import Data.Array (foldl, intercalate, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.String.CodeUnits (length) as S
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..), ElemName(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.CSSLength (CSSLength(..))
import Halogen.Svg.Attributes.Color (Color(..))
import Halogen.Svg.Attributes.FontSize (FontSize(..))
import Halogen.Svg.Attributes.TextAnchor (TextAnchor(..))
import Halogen.Svg.Elements as SE
import Pipeline (JobView, RunView, Status(..), StepView)

statusColor :: Status -> Color
statusColor = case _ of
  Pending -> Named "#9e9e9e"
  Queued -> Named "#ffc107"
  Running -> Named "#2196f3"
  Success -> Named "#4caf50"
  Failure -> Named "#f44336"
  Cancelled -> Named "#ff9800"
  Skipped -> Named "#bdbdbd"

jobH :: Number
jobH = 30.0

jobGap :: Number
jobGap = 8.0

colGap :: Number
colGap = 24.0

padding :: Number
padding = 16.0

labelH :: Number
labelH = 20.0

charW :: Number
charW = 8.0

hPad :: Number
hPad = 16.0

textWidth :: String -> Number
textWidth s = toNumber (S.length s) * charW + hPad

colWidth :: RunView -> Number
colWidth run =
  foldl
    ( \acc j ->
        max acc (textWidth j.name)
    )
    (textWidth run.name)
    run.jobs

colOffset :: Array RunView -> Int -> Number
colOffset runs idx =
  foldl
    ( \acc i ->
        acc + colWidth' i + colGap
    )
    padding
    (range 0 (idx - 1))
  where
  colWidth' i = case index' runs i of
    Nothing -> 0.0
    Just r -> colWidth r

index' :: forall a. Array a -> Int -> Maybe a
index' = Array.index

range :: Int -> Int -> Array Int
range lo hi
  | lo > hi = []
  | otherwise = map (\i -> lo + i) (Array.range 0 (hi - lo))

svgWidth :: Array RunView -> Number
svgWidth runs =
  colOffset runs (length runs) - colGap + padding

svgHeight :: Array RunView -> Number
svgHeight runs =
  let
    maxJobs = foldl (\acc r -> max acc (length r.jobs)) 0
      runs
  in
    padding + labelH + 12.0
      + toNumber maxJobs
          * (jobH + jobGap)
      + padding

renderPipeline
  :: forall w i
   . Array RunView
  -> HH.HTML w i
renderPipeline runs =
  let
    w = svgWidth runs
    h = svgHeight runs
  in
    SE.svg
      [ SA.viewBox 0.0 0.0 w h
      , SA.width w
      , SA.height h
      ]
      (mapWithIndex (renderRun runs) runs)

renderRun
  :: forall w i
   . Array RunView
  -> Int
  -> RunView
  -> HH.HTML w i
renderRun runs colIdx run =
  let
    xOff = colOffset runs colIdx
    w = colWidth run
    sepY = padding + labelH + 6.0
  in
    SE.g []
      ( [ SE.text
            [ SA.x (xOff + w / 2.0)
            , SA.y (padding + labelH)
            , SA.fill (Named "#e0e0e0")
            , SA.fontSize (FontSizeLength (Px 12.0))
            , SA.textAnchor AnchorMiddle
            , SA.class_ (ClassName "run-label")
            ]
            [ HH.text run.name ]
        , SE.line
            [ SA.x1 xOff
            , SA.y1 sepY
            , SA.x2 (xOff + w)
            , SA.y2 sepY
            , SA.stroke (Named "#333")
            , SA.strokeWidth 1.0
            ]
        ]
          <> mapWithIndex
            (renderJob xOff w)
            run.jobs
      )

renderJob
  :: forall w i
   . Number
  -> Number
  -> Int
  -> JobView
  -> HH.HTML w i
renderJob xOff colW rowIdx job =
  let
    boxW = textWidth job.name
    xBox = xOff + (colW - boxW) / 2.0
    yBox = padding + labelH + 12.0 + toNumber rowIdx
      * (jobH + jobGap)
    classes = case job.status of
      Running ->
        [ ClassName "job-node", ClassName "running" ]
      _ -> [ ClassName "job-node" ]
  in
    svgA job.htmlUrl classes
      [ SE.title [] [ HH.text (jobTooltip job) ]
      , SE.rect
          [ SA.x xBox
          , SA.y yBox
          , SA.width boxW
          , SA.height jobH
          , SA.rx 6.0
          , SA.ry 6.0
          , SA.fill (statusColor job.status)
          ]
      , SE.text
          [ SA.x (xBox + boxW / 2.0)
          , SA.y (yBox + jobH / 2.0 + 4.0)
          , SA.fill (Named "#ffffff")
          , SA.fontSize (FontSizeLength (Px 13.0))
          , SA.textAnchor AnchorMiddle
          , SA.class_ (ClassName "job-label")
          ]
          [ HH.text job.name ]
      ]

statusIcon :: Status -> String
statusIcon = case _ of
  Success -> "\x2713"
  Failure -> "\x2717"
  Running -> "\x25B6"
  Pending -> "\x25CB"
  Queued -> "\x25CB"
  Cancelled -> "\x2212"
  Skipped -> "\x2212"

stepLine :: StepView -> String
stepLine s = statusIcon s.status <> " " <> s.name

formatTime :: String -> String
formatTime ts =
  case DS.indexOf (DS.Pattern "T") ts of
    Nothing -> ts
    Just i ->
      let
        afterT = DS.drop (i + 1) ts
      in
        case DS.indexOf (DS.Pattern "Z") afterT of
          Just j -> DS.take j afterT
          Nothing -> afterT

jobTooltip :: JobView -> String
jobTooltip job =
  let
    duration = case job.startedAt, job.completedAt of
      Just s, Just e ->
        formatTime s <> " → " <> formatTime e
      Just s, Nothing -> "started " <> formatTime s
      _, _ -> ""
    steps = map stepLine job.steps
    lines = [ job.name ]
      <> (if duration == "" then [] else [ duration ])
      <>
        ( if Array.null steps then []
          else [ "---" ] <> steps
        )
  in
    intercalate "\n" lines

svgA
  :: forall w i
   . String
  -> Array ClassName
  -> Array (HH.HTML w i)
  -> HH.HTML w i
svgA url classes children =
  SE.element (ElemName "a")
    [ SA.href url
    , SA.classes classes
    ]
    children
