{-# LANGUAGE RecordWildCards #-}
module TFB.Colors (ColorScheme(..), colors1) where

import TFB.Types
import Reflex.Dom.Contrib.Widgets.Shapes (Color(..))

data ColorScheme = ColorScheme{ back, fore :: Color
                              , primary, secondary :: Side -> Color
                              }

colors1 = ColorScheme{..}
  where
    fore = black
    back = white

    primary LeftSide = red
    primary RightSide = blue

    secondary LeftSide = blue
    secondary RightSide = red

black, white, red, blue :: Color
black = "black"
white = "white"
red = "darkRed"
blue = "darkBlue"
