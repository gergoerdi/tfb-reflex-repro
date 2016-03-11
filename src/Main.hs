{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Data.Time.Clock
import Control.Monad.Trans

main = mainWidget $ do
    now <- liftIO getCurrentTime
    tick <- tickLossy 1 now -- clock that ticks once every so often
    rec wealth <- foldDyn (+) 0 (mergeWith (+) [1 <$ click, (-10) <$ buyDrone, droneProd])
        el "div" $ display wealth
        click <- button "Click"
        buyDrone <- fmap (gate (fmap (>= 10) (current wealth))) $ button "Buy Drone (10)"
        drones <- foldDyn (+) 0 (1 <$ buyDrone)
        let droneProd = tagDyn drones tick
    return ()
