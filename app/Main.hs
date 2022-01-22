{-# LANGUAGE TupleSections, FlexibleContexts, TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import WireBoard.Geometry as WBG
import WireBoard.Graphic as WBG
import WireBoard.Util
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad (when)
import System.Exit (die)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (listToMaybe)
import Data.Monoid (getProduct)
--import Text.Printf (printf)
--import Debug.Trace

defaultScreenSize = (1300, 700) -- [pxl]

defaultDisplay :: Display
defaultDisplay = InWindow "WireBoard" defaultScreenSize (100, 100)
--defaultDisplay = FullScreen


--idx2color :: Int -> Color

type AppPointPtr = WBG.PointPtr App


data AppMode
    = NormalMode
    | MovePointMode Parameter


instance Show AppMode where
    show NormalMode = "Normal"
    show (MovePointMode p) = "MovePoint " <> show p


data App = App {
    _mode  :: AppMode,
    _curve :: WBG.Curve
} deriving Show


makeLenses ''App
makePrisms '' AppMode

defaultPointRadius = 5
defaultResolution = 0.02
defaultControlPolygonWidth = 1
defaultCurveWidth = 2
defaultTouchMargin = 2

drawPoint :: WBG.Point -> Picture
drawPoint = draw defaultPointRadius

drawPoints :: [WBG.Point] -> Picture
drawPoints = draw defaultPointRadius

drawPolygon :: WBG.Polygon -> Picture
drawPolygon = draw defaultControlPolygonWidth

drawCurve :: WBG.Curve -> Picture
drawCurve = sdraw defaultResolution defaultCurveWidth


-- Zipper
--findSelectedPoint :: App -> WBG.Point -> AppPointPtr
--findSelectedPoint s c = curve . WBG.findControlPoint (s^.curve) c (defaultPointRadius + defaultTouchMargin)

isTouched :: WBG.Point -> WBG.Point -> Bool
isTouched = WBG.isNeighbor (defaultPointRadius + defaultTouchMargin)

(^?#) :: s -> WBG.PointPtr s -> Maybe WBG.Point
(^?#) s p = (^?) s (cloneTraversal p)


(^?!#) :: s -> WBG.PointPtr s -> WBG.Point
(^?!#) s p = (^?!) s (cloneTraversal p)

(.~#) :: WBG.PointPtr s -> WBG.Point -> s -> s
(.~#) p = (.~) (cloneTraversal p)


---------------------
-- Main
---------------------

initApp :: App
--initApp = App NormalMode $ bezier ps (0,1)
initApp = App NormalMode $ bspline ps 3
--initApp = App NormalMode $ ucb 4 ps
    where
    ps = WBG.regularPolygon 10

drawApp :: App -> IO Picture
drawApp s
    | is _NormalMode $ s ^. mode = drawNormal s
    -- | is _SelectPointMode $ s ^. mode = drawSelectPoint s
    | is _MovePointMode $ s ^. mode = drawMovePoint s


handleInput :: Event -> App -> IO App
handleInput ev s
    | is _NormalMode $ s ^. mode = handleInputNormal ev s
    | is _MovePointMode $ s ^. mode = handleInputMovePoint ev s
    -- | is _SelectPointMode $ s ^. mode = handleInputSelectPoint ev s


stepApp :: Float -> App -> IO App
stepApp _ = return


main :: IO ()
main = do
    --test
    playIO defaultDisplay white 60 initApp drawApp handleInput stepApp



---------------------
-- Common
---------------------

handleInputCommon :: Event -> App -> IO App
handleInputCommon (EventKey (Char 'q') _ _ _) s = die "exit"
handleInputCommon ev s = return s
--print ev >> return s

drawCommon :: App -> Picture
drawCommon s = do
    pictures $ [
        --drawPolygon $ controlPolygon $ s ^. curve,
        --drawPoints $ controlPoints  $ s ^. curve,
        drawCurve $ s ^. curve
        ]



---------------------
-- Normal
---------------------

drawNormal :: App -> IO Picture
drawNormal = return . drawCommon


handleInputNormal :: Event -> App -> IO App
handleInputNormal (EventKey (MouseButton LeftButton) Down _ (x, y)) s = do
    let ps = findParameter (s ^. curve) (point x y) (defaultPointRadius + defaultTouchMargin)
    if length ps > 0
        then return $ s & mode .~ (MovePointMode $ head ps)
        else return $ s & mode .~ NormalMode
--handleInputNormal (EventKey (Char 'a') Down (Modifiers Up Up Up) _) s = return $ s & curve %~ elevate
handleInputNormal ev s = print ev >> handleInputCommon ev s


-----------------------
---- SelectPoint
-----------------------
--
--drawSelectPoint :: App -> IO Picture
--drawSelectPoint s = do
--    let p = s ^?! mode . _SelectPointMode
--    case s ^?# p of
--        Just p -> return $ pictures [ drawCommon s, Color red $ drawPoint p]
--        Nothing -> return $ drawCommon s
--
--
--handleInputSelectPoint :: Event -> App -> IO App
--handleInputSelectPoint (EventKey (MouseButton LeftButton) Down _ (x, y)) s = do
--    let SelectPointMode p = s ^. mode
--    let pnt = s ^?!# p
--    if isTouched pnt (point x y)
--        then return $ s & mode .~ (MovePointMode p)
--        else return $ s & mode .~ NormalMode
--handleInputSelectPoint (EventKey (Char 'a') Down (Modifiers Up Up Up) _) s = return $ s &~ do
--    mode .= NormalMode
--    curve .= elevate (s^.curve)
--handleInputSelectPoint ev s = handleInputCommon ev s



---------------------
-- MovePoint
---------------------

drawMovePoint :: App -> IO Picture
drawMovePoint s = do
    let MovePointMode p = s ^. mode
    let pnt = samplePoint (s ^. curve) p
    return $ pictures [ drawCommon s, Color red $ drawPoint pnt]


handleInputMovePoint :: Event -> App -> IO App
handleInputMovePoint (EventKey (MouseButton LeftButton) Up _ pt) s = do
    let NormalMode = s ^. mode
    return $ s &  mode .~ NormalMode
handleInputMovePoint (EventMotion (x, y)) s = do
    let MovePointMode p = s ^. mode
    return $ s & curve %~ (\c -> modifyWithPoint c p (point x y))
handleInputMovePoint ev s = handleInputCommon ev s


