module TFB.Types
       ( TimeStamp, Side(..), Taps, Tap
       , shiftTaps
       ) where

import Control.Arrow (first)
import Data.List (partition)

type TimeStamp = Double

data Side = LeftSide | RightSide deriving Eq

type Taps = [Tap]
type Tap = (TimeStamp, Side)

shiftTaps :: TimeStamp -> TimeStamp -> Taps -> (Int, Taps)
shiftTaps tolerance t0 ts = (length dropped, map (first $ subtract t0) kept)
  where
    tooLate (t, _) = (t - t0) < -tolerance
    (dropped, kept) = partition tooLate ts
