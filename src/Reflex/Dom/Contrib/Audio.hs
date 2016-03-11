{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}
module Reflex.Dom.Contrib.Audio
       ( AudioCommand(..)
       , audio
       ) where

import Reflex.Dom
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Data.Functor (($>))

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
    playMedia :: HTMLMediaElement -> IO ()

foreign import javascript unsafe "$1.pause()"
    pauseMedia :: HTMLMediaElement -> IO ()

foreign import javascript unsafe "$1.currentTime = $2"
    seekMedia :: HTMLMediaElement -> Double -> IO ()

foreign import javascript unsafe "$1.currentTime"
    getMediaPos :: HTMLMediaElement -> IO Double

audio :: MonadWidget t m => String -> Event t () -> Dynamic t AudioCommand -> m (Event t Double)
audio path sampler cmd = do
    player <- liftIO $ createAudio (fromString path)
    let process cmd = case cmd of
            Play -> liftIO $ playMedia player
            Pause -> liftIO $ pauseMedia player
            Seek timestamp -> liftIO $ seekMedia player timestamp
    schedulePostBuild $ do
        process =<< sample (current cmd)
    addVoidAction $ fmap process (updated cmd)
    performEvent $ sampler $> do
        liftIO $ getMediaPos player
