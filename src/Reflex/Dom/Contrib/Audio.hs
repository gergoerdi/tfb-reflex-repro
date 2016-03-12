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
import GHCJS.DOM.HTMLMediaElement as Media

data AudioCommand = Play
                  | Pause
                  | Seek Double
                  deriving Show

foreign import javascript unsafe "new Audio($1)"
    createAudio :: JSString -> IO HTMLMediaElement

audio :: MonadWidget t m => String -> Event t () -> Dynamic t AudioCommand -> m (Event t Double)
audio path sampler cmd = do
    player <- liftIO $ createAudio (fromString path)
    let process cmd = case cmd of
            Play -> Media.play player
            Pause -> Media.pause player
            Seek timestamp -> Media.setCurrentTime player timestamp
    schedulePostBuild $ do
        process =<< sample (current cmd)
    addVoidAction $ fmap process (updated cmd)
    performEvent $ sampler $> Media.getCurrentTime player
