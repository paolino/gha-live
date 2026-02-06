-- | View — render the pipeline DAG as SVG with Halogen.
module View
  ( renderPipeline
  ) where

import Prelude

import Data.Array (foldl, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..), snd)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Color (Color(..))
import Halogen.Svg.Attributes.CSSLength (CSSLength(..))
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

rowHeight :: Number
rowHeight = 60.0

labelWidth :: Number
labelWidth = 180.0

jobWidth :: Number
jobWidth = 120.0

jobHeight :: Number
jobHeight = 36.0

jobGap :: Number
jobGap = 12.0

padding :: Number
padding = 16.0

renderPipeline
  :: forall w a
   . (String -> a)
  -> Array RunView
  -> HH.HTML w a
renderPipeline onClick runs =
  let
    maxJobs = foldi 0 (\_ acc r -> max acc (length r.jobs)) runs
    svgW = labelWidth + (toNumber maxJobs) * (jobWidth + jobGap) + padding * 2.0
    svgH = (toNumber (length runs)) * rowHeight + padding * 2.0
  in
    SE.svg
      [ SA.viewBox 0.0 0.0 svgW svgH
      , SA.width svgW
      , SA.height svgH
      ]
      (mapWithIndex (renderRun onClick) runs)

renderRun
  :: forall w a
   . (String -> a)
  -> Int
  -> RunView
  -> HH.HTML w a
renderRun onClick rowIdx run =
  let
    yOff = padding + (toNumber rowIdx) * rowHeight + rowHeight / 2.0
  in
    SE.g []
      ( [ SE.text
            [ SA.x (padding + 4.0)
            , SA.y yOff
            , SA.fill (Named "#e0e0e0")
            , SA.fontSize (FontSizeLength (Px 13.0))
            , SA.textAnchor AnchorStart
            , SA.class_ (ClassName "run-label")
            ]
            [ HH.text run.name ]
        ]
          <> mapWithIndex (renderJob onClick rowIdx) run.jobs
      )

renderJob
  :: forall w a
   . (String -> a)
  -> Int
  -> Int
  -> JobView
  -> HH.HTML w a
renderJob onClick rowIdx colIdx job =
  let
    xOff = labelWidth + (toNumber colIdx) * (jobWidth + jobGap)
    yOff = padding + (toNumber rowIdx) * rowHeight + (rowHeight - jobHeight) / 2.0
    classes = case job.status of
      Running -> [ ClassName "job-node", ClassName "running" ]
      _ -> [ ClassName "job-node" ]
  in
    SE.g
      [ HE.onClick (\_ -> onClick job.htmlUrl)
      , SA.classes classes
      ]
      [ SE.rect
          [ SA.x xOff
          , SA.y yOff
          , SA.width jobWidth
          , SA.height jobHeight
          , SA.rx 6.0
          , SA.ry 6.0
          , SA.fill (statusColor job.status)
          ]
      , SE.text
          [ SA.x (xOff + jobWidth / 2.0)
          , SA.y (yOff + jobHeight / 2.0 + 4.0)
          , SA.fill (Named "#ffffff")
          , SA.fontSize (FontSizeLength (Px 11.0))
          , SA.textAnchor AnchorMiddle
          , SA.class_ (ClassName "job-label")
          ]
          [ HH.text job.name ]
      ]

foldi
  :: forall a b
   . b
  -> (Int -> b -> a -> b)
  -> Array a
  -> b
foldi init f arr =
  snd
    ( foldl
        ( \(Tuple idx acc) x ->
            Tuple (idx + 1) (f idx acc x)
        )
        (Tuple 0 init)
        arr
    )
