{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Dom.Contrib.Audio
       ( AudioCommand(..)
       , audio
       ) where

import Reflex.Dom
import Control.Monad.Trans
import Data.Time.Clock as Time

import GHCJS.Types
import Data.String (IsString(..))
import GHCJS.DOM.HTMLMediaElement as Media

data AudioCommand = Play
                  | Pause
                  | Seek Double
                  deriving Show

foreign import javascript unsafe "new Audio($1)"
    createAudio :: JSString -> IO HTMLMediaElement

hold_ :: (MonadHold t m, Reflex t) => Event t a -> m (Behavior t ())
hold_ = hold () . (() <$)

audio :: (MonadWidget t m, MonadIO (PullM t))
      => String -> Dynamic t AudioCommand -> m (Behavior t Double)
audio path cmd = do
    player <- liftIO $ createAudio (fromString path)
    let process cmd = case cmd of
            Play -> Media.play player
            Pause -> Media.pause player
            Seek timestamp -> Media.setCurrentTime player timestamp
    schedulePostBuild $ do
        process =<< sample (current cmd)
    addVoidAction $ fmap process (updated cmd)

    now <- liftIO Time.getCurrentTime
    tick <- hold_ =<< tickLossy (1/1200) now
    return $ pull $ do
        _ <- sample tick
        Media.getCurrentTime player
