module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI rootComponent unit body

rootComponent :: forall q i o m. H.Component q i o m
rootComponent =
  H.mkComponent
    { initialState: \_ -> unit
    , render: \_ -> HH.h1_ [ HH.text "GHA Live" ]
    , eval: H.mkEval H.defaultEval
    }
