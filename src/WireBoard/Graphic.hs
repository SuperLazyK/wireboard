{-# LANGUAGE TemplateHaskell, FlexibleContexts, Rank2Types, TypeSynonymInstances, FlexibleInstances #-}

module WireBoard.Graphic
    ( draw
    , sdraw
    )
where

import WireBoard.Geometry as WBG
import Linear
import Graphics.Gloss as G


class Drawable a where
    draw :: Float -> a -> G.Picture


instance Drawable WBG.Point where
    draw r (V2 x y) = G.translate x y $ G.circleSolid r


instance Drawable WBG.Polygon where
    draw w plg = case points plg of
        []    -> blank
        [pnt] -> draw w pnt
        pnts  -> pictures $ [drawLine w p0 p1 | (p0, p1) <- segs]
            where
            segs = zip pnts (tail pnts)


instance Drawable a => Drawable [a] where
    draw w plys = pictures $ fmap (draw w) plys


class SamplingDrawable a where
    sdraw :: Resolution -> Float -> a -> G.Picture


instance SamplingDrawable a => SamplingDrawable [a] where
    sdraw r w cs = pictures $ sdraw r w <$> cs


instance SamplingDrawable RangedParametric where
    sdraw r w c = draw w $ sampleRangedParametric  r c


instance SamplingDrawable Curve where
    sdraw r w c = sdraw r w $ parametric c


-------------
-- Utility
-------------

wbg2g :: WBG.Point -> G.Point
wbg2g (V2 x y) = (x, y)

drawPoint :: Float -> WBG.Point -> Picture
drawPoint thickness (V2 x y) = G.translate x y $ G.circleSolid thickness

drawLine :: Float -> WBG.Point -> WBG.Point -> Picture
drawLine thickness c0@(V2 x0 y0) c1@(V2 x1 y1)
    | thickness <= 0 = line [(x0, y0), (x1, y1)]
    | otherwise = pictures [ G.polygon [p00, p01, p11, p10]
                           , drawPoint t2 c0
                           , drawPoint t2 c1
                           ]
        where
        t2 = thickness / 2
        v = normalize $ V2 (y1 - y0) (x0 - x1)
        p00 = wbg2g $ c0 + v ^* t2
        p01 = wbg2g $ c0 - v ^* t2
        p10 = wbg2g $ c1 + v ^* t2
        p11 = wbg2g $ c1 - v ^* t2

