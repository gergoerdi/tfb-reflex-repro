{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Data.Time.Clock
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map

main = mainWidget $ do
    now <- liftIO getCurrentTime
    tick <- tickLossy 0.1 now -- clock that ticks once every so often
    time <- mapDyn fst =<< foldDyn (const $ uncurry keepTime) (0, True) tick
    cx <- mapDyn (* 5) time
    fullSVG $ do
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
    return ()

fullSVG :: forall t m a. MonadWidget t m => m a -> m a
fullSVG = svgAttr "svg" $ Map.fromList [ ("width", "100%")
                                       , ("height", "100%")
                                       ]

keepTime :: Int -> Bool -> (Int, Bool)
keepTime x True | x < 20 = (x + 1, True)
                | otherwise = (x - 1, False)
keepTime x False | x > 0 = (x - 1, False)
                 | otherwise = (x + 1, True)
