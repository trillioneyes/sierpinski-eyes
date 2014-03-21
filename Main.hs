{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}
module Main where
import Triangles
import Eyes
import Diagrams.Prelude hiding ((<>), value, option)
import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Options.Applicative

data FractalOpts = MkFOpts {fractalDepth :: Int}

instance Parseable FractalOpts where
  parser = MkFOpts <$> option (long "depth" <> value 7 <> metavar "DEPTH" <> help "The number of fractal iterations to calculate")

main = mainWith makeImage
  where makeImage :: FractalOpts -> Diagram B R2
        makeImage opts = decorated' (fractalDepth opts) greenEye
        greenEye = eye with # scale 0.3
