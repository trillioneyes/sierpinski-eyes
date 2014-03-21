{-# LANGUAGE RecordWildCards #-}
module Eyes(eye,EyeOpts(..)) where

import Diagrams.Prelude
import Data.Default

data EyeOpts = MkEOpts {eyeWidth :: Double,
                        eyeHeight :: Double,
                        irisColor :: Colour Double,
                        pupilSpot :: P2,
                        pupilRadius :: Double,
                        irisRadius :: Double
                        }

instance Default EyeOpts where
  def = MkEOpts {eyeWidth = 1.9,
                 eyeHeight = 1,
                 irisColor = green,
                 pupilSpot = origin,
                 pupilRadius = 0.2,
                 irisRadius = 0.45}

eye MkEOpts{..} = pupil `atop` iris `atop` humor
  where pupil = circle pupilRadius # fc black
        iris = circle irisRadius # fc irisColor
        humor = strokeTrail (glueTrail arcs) # fc white # centerXY # lw 0
        arcs = lid <> lid # reflectX # reflectY
        lid = arcBetween left right (eyeHeight/2)
        left = origin # translateX (-eyeWidth/2)
        right = origin # translateX (eyeWidth/2)
