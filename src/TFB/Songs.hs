module TFB.Songs where

import TFB.Types
import Data.List (sortBy)
import Data.Ord (comparing)

taps1 = sortBy (comparing fst) . concat $
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
