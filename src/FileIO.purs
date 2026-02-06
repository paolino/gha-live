module FileIO
  ( downloadJson
  , pickJsonFile
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

foreign import downloadJsonImpl :: String -> String -> Effect Unit

downloadJson :: String -> String -> Effect Unit
downloadJson = downloadJsonImpl

foreign import pickJsonFileImpl
  :: (String -> Effect Unit) -> Effect Unit -> Effect Unit

pickJsonFile :: Aff String
pickJsonFile = makeAff \cb -> do
  pickJsonFileImpl
    (\s -> cb (pure s))
    (cb (pure ""))
  pure nonCanceler
