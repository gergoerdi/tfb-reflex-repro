{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Reflex.Dom.Contrib.Audio
import Data.Time.Clock
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor (($>))

main :: IO ()
main = mainWidget $ do
    now <- liftIO getCurrentTime
    tick <- tickLossy (1/60) now
    time <- mapDyn fst =<< foldDyn (const $ uncurry keepTime) (0, True) tick
    cx <- mapDyn (* 5) time
    el "div" $ fullSVG $ do
        svgAttr "rect" (Map.fromList [("fill", "lime"),("width", "100%"),("height", "100%")]) $ return ()
        let toAttrs cx = Map.fromList [ ("cx", show cx)
                                      , ("cy", "50")
                                      , ("r", "40")
                                      , ("stroke", "green")
                                      , ("width", "4")
                                      , ("fill", "yellow")
                                      ]
        attrs <- mapDyn toAttrs cx
        svgDynAttr "circle" attrs $ return ()

    timestamp <- el "div" $ do
        play <- fmap (const Play) <$> button "Play"
        pause <- fmap (const Pause) <$> button "Pause"
        rewind <- fmap (const $ Seek 0) <$> button "Rewind"
        cmd <- holdDyn Pause $ leftmost [play, pause, rewind]
        tag <$> audio path cmd <*> pure tick
    el "div" $ do
        display =<< holdDyn 0 timestamp
    return ()
  where
    path = "/home/cactus/prog/ghcjs/stack/tfb/data/song1.mp3"

fullSVG :: forall t m a. MonadWidget t m => m a -> m a
fullSVG = svgAttr "svg" $ Map.fromList [ ("width", "100%")
                                       , ("height", "100%")
                                       ]

keepTime :: Int -> Bool -> (Int, Bool)
keepTime x True | x < 20 = (x + 1, True)
                | otherwise = (x - 1, False)
keepTime x False | x > 0 = (x - 1, False)
                 | otherwise = (x + 1, True)
