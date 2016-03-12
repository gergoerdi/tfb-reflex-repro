{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

import TFB.Types
import TFB.Colors
import TFB.Songs

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Reflex.Dom.Contrib.Widgets.Shapes
import Reflex.Dom.Contrib.Audio
import Data.Time.Clock
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

viewSide :: forall t m. (MonadWidget t m)
         => Side -> ColorScheme -> Dynamic t [TimeStamp] -> m ()
viewSide side ColorScheme{..} ts = do
    dynMoveX (constDyn $ sign * 40) $ do
        label
        target
        marks
  where
    sign = case side of
        LeftSide -> -1
        RightSide -> 1
    size = 40

    label = svgText 30 fore $ case side of LeftSide -> "F"; RightSide -> "J"
    target = scale 1.1 $ outlined (secondary side) $ square size

    marks = void $ do
        ts' <- mapDyn (Map.fromList . zip [(0 :: Int)..] . filter (< horizon)) ts
        listWithKey ts' (const toMark)

    toMark :: Dynamic t TimeStamp -> m ()
    toMark t = do
        let xform body = do
                dx <- mapDyn (\t -> sign * t * 50) t
                s <- mapDyn (\t -> (horizon - t) / horizon) t
                dy <- mapDyn (\t -> sin (t * 0.8) * 50) t
                dynMoveY dy . dynMoveX dx . dynScale s s $ body
        xform $ do
            filled (primary side) shape
            outlined fore shape
      where
        shape = square size

horizon :: TimeStamp
horizon = 5

tolerance :: TimeStamp
tolerance = 0.3

main :: IO ()
main = mainWidget $ do
    now <- liftIO getCurrentTime
    tick <- tickLossy (1/60) now
    time <- mapDyn fst =<< foldDyn (const $ uncurry keepTime) (0, True) tick
    cx <- mapDyn (* 5) time

    timestamp <- el "div" $ do
        play <- fmap (const Play) <$> button "Play"
        pause <- fmap (const Pause) <$> button "Pause"
        rewind <- fmap (const $ Seek 0) <$> button "Rewind"
        cmd <- holdDyn Pause $ leftmost [play, pause, rewind]
        tag <$> audio path cmd <*> pure tick
    timestamp <- holdDyn 0 timestamp
    taps <- mapDyn (\t -> snd $ shiftTaps tolerance t taps1) timestamp
    fullSVG $ do
        viewSide LeftSide colors1 =<< mapDyn (mapMaybe left) taps
        viewSide RightSide colors1 =<< mapDyn (mapMaybe right) taps
    return ()
  where
    path = "/home/cactus/prog/ghcjs/stack/tfb/data/song1.mp3"
    left (ts, side) = do
        guard $ side == LeftSide
        return ts
    right (ts, side) = do
        guard $ side == RightSide
        return ts

fullSVG :: forall t m a. MonadWidget t m => m a -> m a
fullSVG = svgAttr "svg" $ Map.fromList [ ("width", "100%")
                                       , ("height", "100%")
                                       , ("style", "fill: none")
                                       , ("viewBox", "-250 -140 500 280")
                                       ]

keepTime :: (Num a, Ord a) => a -> Bool -> (a, Bool)
keepTime x True | x < 20 = (x + 1, True)
                | otherwise = (x - 1, False)
keepTime x False | x > 0 = (x - 1, False)
                 | otherwise = (x + 1, True)
