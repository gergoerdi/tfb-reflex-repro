{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Contrib.Widgets.Shapes where

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)

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
svgStyleG styles = svgAttr "g" $ Map.fromList [("style", style styles)]

style :: Map String String -> String
style = intercalate ";" . map (\(k, v) -> k ++ ":" ++ v) . Map.toList

dynSVGxformG :: (MonadWidget t m)
             => Dynamic t [String] -> m a -> m a
dynSVGxformG xforms body = do
    attrs <- mapDyn toAttrs xforms
    svgDynAttr "g" attrs body
  where
    toAttrs xforms = Map.fromList [("transform", unwords xforms)]

moveX, moveY :: (MonadWidget t m, Reflex t) => Double -> m a -> m a
moveX = dynMoveX . constDyn
moveY = dynMoveY . constDyn

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
    attrs = Map.fromList [ ("style", style styles)
                         , ("fill", c)
                         , ("x", "0")
                         , ("y", "0")
                         ]
    styles = Map.fromList [ ("alignment-baseline", "central")
                          , ("text-anchor", "middle")
                          , ("font-size", show sz)
                          ]

type Color = String
