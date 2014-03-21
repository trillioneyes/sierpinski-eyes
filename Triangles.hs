{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}
module Triangles(sierpinski,iksnipreis,centers,sizes,decorated,decorated') where
import Diagrams.Prelude hiding (triangle)

triangle = regPoly 3 1

sierpinski 0 = triangle
sierpinski n = (t === (t ||| t) # centerX) # scale (1/2)
  where t = sierpinski (n-1)

iksnipreis 0 = mempty
iksnipreis n = (t === three # centerX) # scale (1/2)
  where t = iksnipreis (n-1) # alignB
        m = triangle # reflectY # alignB
        three = hcat' (with & sep .~ d & catMethod .~ Distrib) [t, m, t]
        d = 1/2

centers = fromVertices . map centroid . pathVertices . iksnipreis
sizes = map sideLength . pathVertices . iksnipreis
  where sideLength (a:b:_) = magnitude (a .-. b)

decorated depth decorations = dec `atop` foreground `atop` background
  where
    dec = decorateTrail (centers depth) dec' # lw 0 # centerXY # translateY d
    foreground = sierpinski depth # fc blue # lw 0 # centerXY
    background = rect 1 (sin (pi/3)) # fc black # centerXY
    dec' = zipWith scale (sizes depth) decorations
    d = -sin (pi/3) / (3 * 2^depth)
decorated' depth dec = decorated depth (repeat dec)
