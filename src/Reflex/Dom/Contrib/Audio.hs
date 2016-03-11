{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}
module Reflex.Dom.Contrib.Audio
       ( AudioCommand(..)
       , audio
       ) where

import Reflex.Dom
import Control.Monad.Trans

import GHCJS.Types
import Data.String (IsString(..))
import GHCJS.DOM.HTMLMediaElement

data AudioCommand = Play
                  | Pause
                  | Seek Double
                  deriving Show

foreign import javascript unsafe "new Audio($1)"
    createAudio :: JSString -> IO HTMLMediaElement

foreign import javascript unsafe "$1.play()"
    playAudio :: HTMLMediaElement -> IO ()

foreign import javascript unsafe "$1.pause()"
    pauseAudio :: HTMLMediaElement -> IO ()

foreign import javascript unsafe "$1.currentTime = $2"
    seekAudio :: HTMLMediaElement -> Double -> IO ()

foreign import javascript unsafe "$1.currentTime"
    audioPos :: HTMLMediaElement -> IO Double

-- audio :: MonadWidget t m => String -> Dynamic t AudioCommand -> m (Dynamic t Double)
audio :: MonadWidget t m => String -> Dynamic t AudioCommand -> m ()
audio path cmd = do
    player <- liftIO $ createAudio (fromString path)
    let process cmd = case cmd of
            Play -> liftIO $ playAudio player
            Pause -> liftIO $ pauseAudio player
            Seek timestamp -> liftIO $ seekAudio player timestamp
    schedulePostBuild $ do
        process =<< sample (current cmd)
    addVoidAction $ fmap process (updated cmd)
