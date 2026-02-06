-- | View — render the pipeline DAG as SVG with Halogen.
module View
  ( renderPipeline
  ) where

import Prelude

import Data.Array as Array
import Data.Array (foldl, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (length) as S
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.CSSLength (CSSLength(..))
import Halogen.Svg.Attributes.Color (Color(..))
import Halogen.Svg.Attributes.FontSize (FontSize(..))
import Halogen.Svg.Attributes.TextAnchor (TextAnchor(..))
import Halogen.Svg.Elements as SE
import Pipeline (JobView, RunView, Status(..))

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
charW = 7.0

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
  :: forall w a
   . (String -> a)
  -> Array RunView
  -> HH.HTML w a
renderPipeline onClick runs =
  let
    w = svgWidth runs
    h = svgHeight runs
  in
    SE.svg
      [ SA.viewBox 0.0 0.0 w h
      , SA.width w
      , SA.height h
      ]
      (mapWithIndex (renderRun onClick runs) runs)

renderRun
  :: forall w a
   . (String -> a)
  -> Array RunView
  -> Int
  -> RunView
  -> HH.HTML w a
renderRun onClick runs colIdx run =
  let
    xOff = colOffset runs colIdx
    w = colWidth run
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
        ]
          <> mapWithIndex
            (renderJob onClick xOff w)
            run.jobs
      )

renderJob
  :: forall w a
   . (String -> a)
  -> Number
  -> Number
  -> Int
  -> JobView
  -> HH.HTML w a
renderJob onClick xOff colW rowIdx job =
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
    SE.g
      [ HE.onClick (\_ -> onClick job.htmlUrl)
      , SA.classes classes
      ]
      [ SE.rect
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
          , SA.fontSize (FontSizeLength (Px 11.0))
          , SA.textAnchor AnchorMiddle
          , SA.class_ (ClassName "job-label")
          ]
          [ HH.text job.name ]
      ]
