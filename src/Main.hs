{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Reflex.Dom.Contrib.Audio
import Data.Time.Clock
import Control.Monad
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor (($>))
import Data.List (intercalate)
import Text.Printf
import Data.Maybe (mapMaybe)
import Data.List (sortBy, partition)
import Data.Ord (comparing)
import Control.Arrow (first)

rectangle w h = dynRectangle (constDyn w) (constDyn h)

dynRectangle :: (MonadWidget t m)
             => Dynamic t Double -> Dynamic t Double -> m ()
dynRectangle w h = do
    attrs <- mapDyn (uncurry toAttrs) =<< combineDyn (,) w h
    svgDynAttr "rect" attrs $ return ()
  where
    toAttrs w h = Map.fromList [ ("x", show $ negate $ w / 2)
                               , ("width", show w)
                               , ("y", show $ negate $ h / 2)
                               , ("height", show h)
                               ]

dynSquare s = dynRectangle s s

square :: (MonadWidget t m, Reflex t) => Double -> m ()
square = dynSquare . constDyn

outlined s = svgStyleG $ Map.fromList [("stroke", s)]

filled s = svgStyleG $ Map.fromList [("fill", s)]

svgStyleG :: (MonadWidget t m)
          => Map String String -> m a -> m a
svgStyleG styles = svgAttr "g" $ Map.fromList [("style", style)]
  where
    style = intercalate ";" $ map (\(k, v) -> k ++ ":" ++ v) $ Map.toList styles

dynSVGxformG :: (MonadWidget t m)
             => Dynamic t [String] -> m a -> m a
dynSVGxformG xforms body = do
    attrs <- mapDyn toAttrs xforms
    svgDynAttr "g" attrs body
  where
    toAttrs xforms = Map.fromList [("transform", unwords xforms)]

dynMoveX :: (MonadWidget t m, Reflex t) => Dynamic t Double -> m a -> m a
dynMoveX dx body = do
    xforms <- mapDyn ((:[]) . toXform) dx
    dynSVGxformG xforms body
  where
    toXform x = unwords ["translate(", show x, "0", ")"]

dynMoveY :: (MonadWidget t m, Reflex t) => Dynamic t Double -> m a -> m a
dynMoveY dy body = do
    xforms <- mapDyn ((:[]) . toXform) dy
    dynSVGxformG xforms body
  where
    toXform y = unwords ["translate(", "0", show y, ")"]

dynScale :: (MonadWidget t m, Reflex t) => Dynamic t Double -> Dynamic t Double -> m a -> m a
dynScale dsx dsy body = do
    xforms <- mapDyn ((:[]) . uncurry toXform) =<< combineDyn (,) dsx dsy
    dynSVGxformG xforms body
  where
    toXform sx sy = unwords ["scale(", show sx, show sy, ")"]

scale :: (MonadWidget t m, Reflex t) => Double -> m a -> m a
scale s = dynScale (constDyn s) (constDyn s)

dynScaleX :: (MonadWidget t m, Reflex t) => Dynamic t Double -> m a -> m a
dynScaleX dsx = dynScale dsx (constDyn 1)

scaleX :: (MonadWidget t m, Reflex t) => Double -> m a -> m a
scaleX = dynScaleX . constDyn

svgText :: (MonadWidget t m) => Double -> Color -> String -> m ()
svgText sz c s = svgAttr "text" attrs $ text s
  where
    attrs = Map.fromList [ ("style", printf "font-size:%f; alignment-baseline:central; text-anchor: middle" sz)
                         , ("fill", c)
                         , ("x", "0")
                         , ("y", "0")
                         ]

data Side = LeftSide | RightSide deriving Eq
type Color = String
data ColorScheme = ColorScheme{ back, fore :: Color
                              , primary, secondary :: Side -> Color
                              }
type TimeStamp = Double

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

colors1 = ColorScheme{ fore = black
                     , back = white
                     , primary = \s -> case s of
                         LeftSide -> red
                         RightSide -> blue
                     , secondary = \s -> case s of
                         LeftSide -> blue
                         RightSide -> red
                     }

black = "black"
white = "white"
red = "darkRed"
blue = "darkBlue"

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
    -- el "div" $ do
    --     display timestamp

    -- el "div" $ fullSVG $ do
    --     svgAttr "rect" (Map.fromList [("fill", "lime"),("width", "100%"),("height", "100%")]) $ return ()
    --     -- let toAttrs cx = Map.fromList [ ("cx", show cx)
    --     --                               , ("cy", "50")
    --     --                               , ("r", "40")
    --     --                               , ("stroke", "green")
    --     --                               , ("width", "4")
    --     --                               , ("fill", "yellow")
    --     --                               ]
    --     -- attrs <- mapDyn toAttrs cx
    --     -- svgDynAttr "circle" attrs $ return ()
    --     -- outlined "red" $ dynRectangle timestamp cx
    --     dynMoveX timestamp $ outlined "red" $ rectangle 100 100
    taps <- mapDyn (\t -> snd $ shiftTaps t taps) timestamp
    fullSVG $ do
        viewSide LeftSide colors1 =<< mapDyn (mapMaybe left) taps
        viewSide RightSide colors1 =<< mapDyn (mapMaybe right) taps
        -- filled red $ rectangle 100 100
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

type Taps = [Tap]
type Tap = (TimeStamp, Side)

shiftTaps :: TimeStamp -> Taps -> (Int, Taps)
shiftTaps t0 ts = (length dropped, map (first $ subtract t0) kept)
  where
    tooLate (t, _) = (t - t0) < -tolerance
    (dropped, kept) = partition tooLate ts

taps = sortBy (comparing fst) . concat $
       [ intro 8

       , right <$> steps 24 2
       , left <$> steps 26 2
       , right <$> steps 28 2
       , left <$> steps 30 2

       , right <$> syncope 32
       , right <$> syncope 34
       , left <$> syncope 36
       , left <$> syncope 38
       , right <$> syncope 40
       , left <$> syncope 42
       , right <$> syncope 44
       , left <$> syncope 46

       , left <$> [ 48 ]
       , right <$> syncope 48
       , left <$> [ 50 ]
       , right <$> syncope 50
       , left <$> [ 52 ]
       , right <$> syncope 52
       , left <$> [ 54 ]
       , right <$> syncope 54

       , right <$> [ 56 ]
       , left <$> syncope 56
       , right <$> [ 58 ]
       , left <$> syncope 58
       , right <$> [ 60 ]
       , left <$> syncope 60
       , right <$> [ 62 ]
       , left <$> syncope 62

       , left <$> [ 64, 65.5 ]
       , right <$> take 3 (syncope 64)

       , left <$> [ 66, 67.5 ]
       , right <$> take 3 (syncope 66)

       , right <$> [ 68, 69.5 ]
       , left <$> take 3 (syncope 68)
       , right <$> [ 70, 71.5 ]
       , left <$> take 3 (syncope 70)

       , left <$> [ 72, 73.5 ]
       , right <$> take 3 (syncope 72)
       , left <$> [ 74, 75.5 ]
       , right <$> take 3 (syncope 74)

       , right <$> [ 76, 77.5 ]
       , left <$> take 3 (syncope 76)
       , right <$> [ 78, 79.5 ]
       , left <$> take 3 (syncope 78)

       , intro 80
       , intro 96

       , right <$> syncope 112
       , left <$> syncope 114
       , right <$> syncope 116
       , left <$> [ 118, 119 ]
       , right <$> syncope 120
       , left <$> syncope 122
       , right <$> syncope 124
       , left <$> [ 126, 127 ]

       , right <$> melody 128
       , left <$> melody 136

       , right <$> melody 144
       , left <$> steps' 144 8
       , left <$> melody 152
       , right <$> steps' 152 8

       , right <$> melody 160
       , left <$> steps 160 8
       , left <$> melody 168
       , right <$> steps 168 8

       , right <$> steps 176 8
       , right <$> steps 184 8
       , left <$> steps 184 8
       , left <$> steps 192 8
       , right <$> steps 200 8
       , left <$> steps 200 8
       ]
  where
    bpm = 140
    residual = 0
    slope = 2 * 60 / bpm
    beat i = residual + i * slope
    right i = (beat i, RightSide)
    left i = (beat i, LeftSide)
    steps from len = enumFromThenTo from (from + 1) (from + len - 1)
    steps' from len = enumFromThenTo from (from + 2) (from + len - 1)
    syncope from = [from, from + 0.75, from + 1, from + 1.5]
    melody from = [ from, from + 1.5, from + 1.75, from + 2
                  , from + 3.5, from + 4, from + 5.5, from + 6]
    intro from = concat
                   [ right <$> steps from 8
                   , left <$> steps (from + 8) 8
                   ]
