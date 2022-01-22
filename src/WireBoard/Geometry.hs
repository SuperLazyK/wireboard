{-# LANGUAGE TupleSections, FlexibleContexts, TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, GADTs, StandaloneDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module WireBoard.Geometry
    ( Point
    , Polygon
    , Parameter
    , PointPtr
    , point
    , points
    , polygon
    , lineseg
    , Parametric
    , RangedParametric
    , Range
    , Resolution
    , ControlCurve(..)
    , Curve
    , allBernstein
    , regularPolygon
    -- , blossom
    , sampleRangedParametric
    , bezier
    , bspline
    , ucb
    , test
    , isNeighbor)
where

import Linear (V2(..), (*^), norm)
import Data.Maybe (listToMaybe)
import Data.List (findIndices)
import Data.Map as M (insert, lookup, Map, empty)
import Control.Lens
import Debug.Trace
import WireBoard.Util (assert, extract, assertM)
import Data.Function (on)
import Control.Monad (when, forM_)
import Control.Monad.State (evalState, State, modify, get)


fdiv = on (/) fromIntegral

------------------------------
-- Point
------------------------------

type Point = V2 Float

point :: Float -> Float -> Point
point = V2


isNeighbor :: Float -> Point -> Point -> Bool
isNeighbor r c p = norm (p - c) <= r


findNeighborPointIdx :: [Point] -> Point -> Float -> Int
findNeighborPointIdx ps c r = case findIndices (isNeighbor r c) ps of
        [] -> -1
        (i:_) -> i


class HasPoints a where
    points :: a -> [Point]


------------------------------
-- Polygon
------------------------------

newtype Polygon = Polygon [Point] deriving Show

makeWrapped ''Polygon

lineseg :: Point -> Point -> Polygon
lineseg p1 p2 = polygon [p1, p2]


polygon :: [Point] -> Polygon
polygon = Polygon


instance HasPoints Polygon where
    points (Polygon xs) = xs


regularPolygon :: Int -> [Point]
regularPolygon n = if even n then ps else error "not even"
    where
    theta2 = pi / fromIntegral n
    ps =  [f i | i <- [0..(2*n-1)]] ++ [f 0]
    f i = if even i
            then innerP $ (fromIntegral i) * theta2
            else outerP $ (fromIntegral i) * theta2
    outerR = 200
    innerR = outerR * cos theta2
    innerP th =  point (innerR * cos th) (innerR * sin th)
    outerP th =  point (outerR * cos th) (outerR * sin th)



------------------------------
-- AABB
------------------------------

data AABB = AABB Float Float Float Float deriving Show -- xmin ymin xmax ymax


aabb :: [Point] -> AABB
aabb (p:ps) = foldr unionAABB (singletonAABB p) ps
aabb [] = error "empty AABB"


singletonAABB :: Point -> AABB
singletonAABB (V2 x y) = AABB x y x y


unionAABB :: Point -> AABB -> AABB
unionAABB (V2 x y) (AABB xmin ymin xmax ymax) = AABB (min xmin x) (min ymin y) (max xmax x) (max ymax y)


inAABB :: Point -> AABB -> Bool
inAABB (V2 x y) (AABB xmin ymin xmax ymax) = xmin <= x && x <= xmax && ymin <= y && y <= ymax


instance HasPoints AABB where
    points (AABB xmin ymin xmax ymax) = [p1, p2, p3, p4]
        where
        p1 = point xmin ymin
        p2 = point xmax ymin
        p3 = point xmin ymax
        p4 = point xmax ymax



------------------------------
-- Parametric
------------------------------

type Parameter = Float

type Range = (Parameter, Parameter) -- [a, b]

type Resolution = Float

type Parametric = Parameter -> Point


sampleRange :: Resolution -> Range -> [Parameter]
sampleRange resolution (b,e) = if abs (e - b) < resolution then [] else ts
    where
    ts = filter (\t -> t <= e) [b,(b + resolution)..e]


sampleParametric :: Resolution -> Parametric -> Range -> Polygon
sampleParametric resolution f r = polygon $ f <$> sampleRange resolution r


data RangedParametric = RangedParametric {
    _paramf :: Parametric,
    _domain :: [Range]
}

makeLenses ''RangedParametric


isValidInterval :: Range -> Parameter -> Bool
isValidInterval (k1,k2) p = k1 < k2 && k1 <= p && p <= k2


sampleRangedParametric :: Resolution -> RangedParametric -> [Polygon]
sampleRangedParametric resolution (RangedParametric f rs) = sampleParametric resolution f <$> rs


unionRangedParametric :: [RangedParametric] -> RangedParametric
unionRangedParametric rps = RangedParametric f (concat rss)
    where
    rss = [rp ^. domain | rp <- rps]
    fs = [rp ^. paramf | rp <- rps]
    f p = assert (idx >= 0) "idxshow" $ (fs !! idx) p
        where
        [idx] = findIndices g rss
        g = any $ flip isValidInterval p


paramCombination :: Int -> Range -> [[Parameter]]
paramCombination n (u1,u2) = [replicate (n-i) u1 <> replicate i u2  | i <- [0..n]]


------------------------------
-- Interpolation
------------------------------
class Weightable a where
    weight ::  Float -> a -> a
    weightedSum :: (Num a) => [Float] -> [a] -> a
    weightedSum ws ps = foldr1 (+) $ zipWith weight ws ps


instance Weightable Point where
    weight = (*^)


instance Weightable Float where
    weight = (*)



------------------------------
-- ControlCurve
------------------------------

type Knot = Parameter

type PointPtr a = ATraversal' a Point


class ControlCurve a where
    controlDomain :: a -> Range
    controlPolygon :: a -> Polygon
    controlPoints :: a -> [Point]
    parametric :: a -> RangedParametric
    samplePoint :: a -> Parameter -> Point
    samplePoint c = rp ^. paramf
        where
        rp = parametric c
    --findControlPoint :: a -> Point -> Float -> PointPtr a
    findParameter :: a -> Point -> Float -> [Parameter]
    --elevate :: a -> a
    modifyWithPoint :: a -> Parameter -> Point -> a


data Curve where
    Curve :: forall c. (ControlCurve c, Show c) => c -> Curve

deriving instance Show Curve

makePrisms ''Curve


instance ControlCurve Curve where
    controlDomain (Curve c) = controlDomain c
    controlPolygon (Curve c) = controlPolygon c
    controlPoints (Curve c) = controlPoints c
    parametric (Curve c) = parametric c
    --elevate (Curve c) =  Curve $ elevate c
    --findControlPoint (Curve c) p r = _Curve . findControlPoint c p r
    findParameter (Curve c) p r = findParameter c p r
    modifyWithPoint (Curve c) t p = Curve $ modifyWithPoint c t p



------------------------------
-- Bezier
------------------------------

blossom :: [Point] -> Range -> [Parameter] -> Point
blossom ps (a,b) ts = flip evalState M.empty $ m
    where
    delta = 1 / (b - a)
    ts' = (\t -> (t -a) * delta) <$> ts
    m = decasteliau ps ts' (length ps - 1) 0



decasteliau :: (Num a, Weightable a) => [a] -> [Parameter] -> Int -> Int -> State (M.Map (Int, Int) a) a
decasteliau ps _ 0 i = return $ assert (i >= 0 ) "index-check" $ ps !! i
decasteliau ps ts r i = do
    memo <- get
    case M.lookup (r, i) memo of
        Just p -> return p
        Nothing -> do
            let t = ts !! (r-1)
            d1 <- decasteliau ps ts (r-1) i
            d2 <- decasteliau ps ts (r-1) (i+1)
            let d3 = weightedSum [1 - t, t] [d1, d2]
            modify $ M.insert (r,i) d3
            return d3


bernstein :: [Parameter] -> Int -> Int -> State (M.Map (Int, Int) Parameter) Parameter
bernstein _ 0 i = if (i == 0) then return 1 else return 0
bernstein ts r i = if (i < 0 && i > r)
        then return 0
        else do
            let t = ts !! (r-1)
            memo <- get
            case M.lookup (r, i) memo of
                Just p -> return p
                Nothing -> do
                    d1 <- bernstein ts (r-1) i
                    d2 <- bernstein ts (r-1) (i-1)
                    let d3 = weightedSum [1 - t, t] [d1, d2]
                    modify $ M.insert (r,i) d3
                    return d3

-- some cost to calc all basis for a given parameter
allBernstein :: Int -> Parameter -> [Parameter]
allBernstein n t = flip evalState M.empty $ do
    mapM (bernstein (replicate n t) n) [0..n]


data Bezier = Bezier [Point] Range deriving Show

makePrisms ''Bezier


instance ControlCurve Bezier where
    controlDomain (Bezier _ r) = r
    controlPolygon (Bezier ps _) = Polygon ps
    controlPoints (Bezier ps _) = ps
    parametric c@(Bezier _ r) = RangedParametric (sampleBezier c) [r]
    --findControlPoint (Bezier ps _) c r = _Bezier . _1 . (ix $ findNeighborPointIdx ps c r)
    findParameter = findParameterBezier
    --elevate = elevateBezier
    modifyWithPoint = modifyWithPointBezier


sampleBezier :: Bezier -> Parametric
sampleBezier (Bezier [] _) = error "no control point"
sampleBezier (Bezier ps (a,b)) = \t -> blossom ps (a, b) (replicate (length ps -1) t)


bisectBezier :: Bezier -> [Bezier]
bisectBezier c@(Bezier ps (a,b)) = [c1, c2]
    where
    cp = (a + b) / 2
    n = length ps - 1
    ps1 = [blossom ps (a, b) params | params <- paramCombination n (a, cp)]
    ps2 = [blossom ps (a, b) params | params <- paramCombination n (cp, b)]
    c1 = Bezier ps1 (a, cp)
    c2 = Bezier ps2 (cp, b)


traverseBezier :: (Bezier -> Bool) -> (Bezier -> Bool) -> (Bezier -> [Bezier]) -> Bezier -> [Bezier]
traverseBezier canTerminate maybeTarget split c = f [c] []
    where
    f [] acc = acc
    f (c:cs) acc = if maybeTarget c
        then if canTerminate c
            then f cs (c:acc)
            else f ((split c) ++ cs) acc
        else f cs acc
        where
        d = c ^?! _Bezier . _2
        r = aabb $ c ^?! _Bezier . _1

centerBezier :: Bezier -> Parameter
centerBezier (Bezier _ (a,b)) = (a + b) / 2


aabbBezier :: Bezier -> AABB
aabbBezier (Bezier ps _) = aabb ps


findParameterBezier :: Bezier -> Point -> Float -> [Parameter]
findParameterBezier c p r = centerBezier <$> cs
    where
    cs = traverseBezier canTerminate maybeTarget bisectBezier c
    canTerminate c' = all (isNeighbor r p) [p' | p' <- points $ aabbBezier c']
    maybeTarget c' = p `inAABB` aabbBezier c'


-- degree elevation
elevateBezier :: Bezier -> Bezier
elevateBezier (Bezier ps r) = Bezier ps' r
    where
    n = length ps - 1
    go i n = (i `fdiv` (n+1)) *^ (ps !! (i-1)) + ((n + 1 - i) `fdiv` (n+1)) *^ (ps !! i)
    ps' = [ps !! 0] <> [ go i n | i <- [1..n] ] <> [ps !! n]


-- for closed curve
modifyWithPointBezier :: Bezier -> Parameter -> Point -> Bezier
modifyWithPointBezier c@(Bezier ps r) t p = Bezier ps' r
    where
    p0 = samplePoint c t
    dx = p - p0
    n = length ps - 1
    va = case allBernstein n t of
        (b:bs) -> b + (last bs) : init bs
        _ -> error "empty list"
    a = foldr1 (+) $ zipWith (*) va va
    dps :: [Point]
    dps = fmap (*^ ((1/a) *^ dx)) va
    ps' = zipWith (+) (dps ++ [head dps]) ps


bezier ::  [Point] -> Range -> Curve
bezier ps r = Curve $ Bezier ps r


------------------------------
-- BSpline
------------------------------


data BSpline = BSpline [Point] [Knot] deriving Show

makePrisms ''BSpline


lkn :: [Point] -> [Knot] -> (Int, Int, Int)
lkn ps knots = (l, k, n)
    where
    l = length ps - 1
    k = length knots - 1
    n = k + 1 - l


domainBSpline :: [Point] -> [Knot] -> Range
domainBSpline ps knots = (a, b)
    where
    (_, k, n) = lkn ps knots
    a = knots !! (n - 1)
    b = knots !! (k - n + 1)


validateBSpline ::  BSpline -> BSpline
validateBSpline c@(BSpline ps knots) = traceShow (l,k,n)
        $ assert (l < k) ("L < K", l, k)
        $ assert (k >= 2 * n - 1) ("K >= 2 * n - 1", k, n)
        $ assert (a < b) ("invalid domain", a, b)
        $ c
        where
        (l, k, n) = lkn ps knots
        (a, b) = controlDomain c


instance ControlCurve BSpline where
    controlDomain c@(BSpline ps knots) = domainBSpline ps knots
    controlPolygon (BSpline ps _) = Polygon ps
    controlPoints (BSpline ps _) = ps
    parametric = parametricBSpline
    --findControlPoint (BSpline ps _) c r =  _BSpline . _1 . (ix $ findNeighborPointIdx ps c r)
    findParameter = findParameterBSpline
    --elevate = id
    modifyWithPoint = modifyWithPointBSpline


parametricBSpline :: BSpline -> RangedParametric
parametricBSpline c@(BSpline ps knots) = RangedParametric f [controlDomain c]
    where
    (_,_,n) = lkn ps knots
    f t = blossomBSpline c (replicate n t)


findKnotIdx :: [Knot] -> [Parameter] -> Int
findKnotIdx [] _ = error "invalid knots"
findKnotIdx [k] _ = error "out-of-range : len(knots)=1"
findKnotIdx knots ps = case indeces of
    [] -> error "out-of-range (idx not found)"
    (i:_) -> i
    where
    indeces = findIndices id $ zipWith f knots (tail knots)
    f k k' = all (\p -> k <= p && p <= k') ps


blossomBSpline :: BSpline -> [Parameter] -> Point
blossomBSpline c@(BSpline ps knots) ts = id
        $ assert (all (\t -> isValidInterval (controlDomain c) t) ts) ("checkRange", (controlDomain c), ts)
        $ assert (l == k - n + 1) ("lkn", l, n, k)
        $ assert (l == n) ("l == n", l, n, idx, length ps)
        $ assert (2 * n == k + 1) ("2*n == k + 1", l, n)
        $ traceShow (l, k, n, length ts) $ blossomWithKnots ps' knots' ts
        where
        (_, _, n) = lkn ps knots
        idx :: Int
        idx = findKnotIdx knots ts
        knots' = extract (idx - n + 1) (2*n)  knots
        ps' = extract (idx - n + 1) (n + 1) ps
        k = length knots' - 1
        l = length ps' - 1


-- each knot should be different to each other
-- knots is sorted
blossomWithKnots :: [Point] -> [Knot] -> [Parameter] -> Point
blossomWithKnots ps knots ts = id
           $ assert (n + 1 == length ps) ("n + 1 == length ps", n, length ps)
           $ assert (2 * n == length knots) ("2 * n == length knots", length knots, n)
           $ assert (n >= 1) ("n >= 1", n)
           $ assert (and $ zipWith (<) knots (tail knots)) ("monotonic increae", knots)
           $ assert (n < k) "n < k"
           $ assert (all (isValidInterval (k1,k2))  ts) ("param range", k1, ts, k2, knots)
           $ flip evalState M.empty $ m
    where
    n = length ts
    k = length knots
    k1 = knots !! (n-1)
    k2 = knots !! n
    m = deboor ps knots ts n 0


-- decastoro -= knots == 000111
deboor :: (Num a, Weightable a) => [a] -> [Knot] -> [Parameter] -> Int -> Int -> State (M.Map (Int, Int) a) a
deboor ps     _  _ 0 i = do
    assertM (i >= 0 && i < length ps) ("index-check", i, length ps)
    return $ ps !! i
deboor ps knots ts r i = do
    memo <- get
    case M.lookup (r, i) memo of
        Just p -> return p
        Nothing -> do
            let n = length ts
            let k = length knots
            let u = ts !! (n - r)
            let a =  assert (r + i - 1 < k) "r + i < k" $ knots !! (r + i - 1)
            let b =  assert (n + i < k) ("n + i < k", n, i, k) $ knots !! (n + i)
            let t = (u - a) / (b - a)  -- localParam
            d1 <- deboor ps knots ts (r-1) i
            d2 <- deboor ps knots ts (r-1) (i+1)
            let d3 = weightedSum [1 - t, t] [d1, d2]
            modify $ M.insert (r,i) d3
            return d3



bspline2bezier :: BSpline -> [Bezier]
bspline2bezier c@(BSpline ps knots) = [Bezier (f r) r |r <- rs]
    where
    (_, _, n) = lkn ps knots
    (a, b) = controlDomain c
    ks = filter (\x -> a <= x && x <= b) knots -- ?? fail for  <= b
    rs = zip ks (tail ks)
    f r = [blossomBSpline c params | params <- paramCombination n r]



findParameterBSpline :: BSpline -> Point -> Float -> [Parameter]
findParameterBSpline c p r = concat [findParameterBezier b p r | b <- bs]
    where
    bs = bspline2bezier c


-- assume uniform interval knot
bspline ::  [Point] -> Int -> Curve
bspline ps n = Curve $ bspline' ps n

bspline' ::  [Point] -> Int -> BSpline
bspline' ps n = BSpline ps knots
    where
    l = length ps - 1
    k = l + n - 1
    delta = 1 / (fromIntegral k)
    knots = [0,delta..1]


knotRange :: Int -> Int -> (Int, Int)
knotRange r i = (i-1, i + r - 1)


basisCoeffL :: Parameter -> [Knot] -> Int -> Int -> Parameter
basisCoeffL u knots r i = if
    | li <  0     -> 0 -- if u == b then 1 else 0
    | ui >= k     -> 0
    | otherwise   -> (u - a) / (b - a)
    where
    k = length knots
    (li,ui) = knotRange r i
    a = knots !! li
    b = knots !! ui


basisCoeffR :: Parameter -> [Knot] -> Int -> Int -> Parameter
basisCoeffR u knots r i =  if
    | li <  0     -> 0
    | ui >= k     -> if u == a then 1 else 0
    | otherwise   -> (b - u) / (b - a)
    where
    k = length knots
    (li,ui) = knotRange r i
    a = knots !! li
    b = knots !! ui


basisBSpline :: Parameter -> [Knot] -> Int -> Int -> State (M.Map (Int, Int) Parameter) Parameter
basisBSpline u knots 0 i = return $ if
        | i <  1     -> 0
        | i == k     -> if u == k0 then 1 else 0
        | otherwise  -> if (k0 <= u && u < k1) then 1 else 0
        where
        k = length knots
        k0 = knots !! (i-1)
        k1 = knots !! i
basisBSpline u  knots r i = do
            let k = length knots
            memo <- get
            case M.lookup (r, i) memo of
                Just p -> return p
                Nothing -> do
                    let t1 = basisCoeffL u knots r i
                    let t2 = basisCoeffR u knots r (i+1)
                    d1 <- basisBSpline u knots (r-1) i
                    d2 <- basisBSpline u knots (r-1) (i+1)
                    let d3 = weightedSum [t1, t2] [d1, d2]
                    modify $ M.insert (r,i) d3
                    return d3

-- some cost to calc all basis for a given parameter
allBasisBSpline :: Int -> [Knot] -> Parameter -> [Parameter]
allBasisBSpline n knots t = flip evalState M.empty $ do
    let k = length knots - 1
    let l = k - n + 1
    mapM (basisBSpline t knots n) [0..l]


modifyWithPointBSpline :: BSpline -> Parameter -> Point -> BSpline
modifyWithPointBSpline c@(BSpline ps knots) t p = BSpline ps' knots
    where
    p0 = samplePoint c t
    dx = p - p0
    (_,_,n) = lkn ps knots
    va = allBasisBSpline n knots t
    a = foldr1 (+) $ zipWith (*) va va
    dps :: [Point]
    dps = fmap (*^ ((1/a) *^ dx)) va
    ps' = zipWith (+) (dps) ps


test :: IO ()
test = do
    let ps = regularPolygon 4
    let c = bspline' ps 3
    --print $ samplePoint c 0.2
    --print $ controlDomain c
    let (a, b) = controlDomain c
    print $ blossomBSpline c [b, b, b]
    return ()


-----------------------------------------
-- Uniform Knot Interval Closed BSpline
-----------------------------------------

-- #Knot = #ControlPoint
data UCB = UCB [Point] Int deriving Show


makePrisms ''UCB


domainUCB :: Range
domainUCB = (0, 1)


validateUCB ::  UCB -> UCB
validateUCB c@(UCB ps n) = id
        $ assert (n > 0) ("n > 0", n)
        $ assert (length ps >= n) ("#control point >= n", length ps, n)
        $ c


instance ControlCurve UCB where
    controlDomain _ = domainUCB
    controlPolygon (UCB ps _) = Polygon ps
    controlPoints (UCB ps _) = ps
    parametric = parametricUCB
    findParameter = findParameterUCB
    modifyWithPoint = modifyWithPointUCB



ucb ::  Int -> [Point] -> Curve
ucb n ps = Curve $ validateUCB $ UCB ps n


parametricUCB :: UCB -> RangedParametric
parametricUCB c@(UCB ps n) = RangedParametric f [domainUCB]
    where
    f t = blossomUCB c $ replicate n t


blossomUCB :: UCB -> [Parameter] -> Point
blossomUCB (UCB ps n) ts@(t:_) = id
        $ assert (isValidInterval domainUCB t) "checkRange"
        $ assert (n + 1 == length ps'') ("n + 1 == length ps''", n, length ps'')
        $ blossomWithKnots ps'' knots' ts
        where
        l = length ps
        idx :: Int
        idx = floor (t * fromIntegral l)
        delta = (fromIntegral 1 :: Float) / (fromIntegral l :: Float)
        knots = [delta * fromIntegral i | i <-  [-n + 1 .. l + n]] -- to handle overlapped interpolation range, assynmetry due to right-hand-endpoint
        knots' = extract idx (2*n) knots
        ps' = let k = (if even n then n else (n-1)) `div` 2 in (drop (l-k) ps) ++ ps ++ (take (k+2) ps) -- k+2: last segment
        ps'' = extract idx (n+1) ps'



ucb2bezier :: UCB -> [Bezier]
ucb2bezier c@(UCB ps n) = [Bezier (f r) r |r <- rs]
    where
    l = length ps
    delta = (fromIntegral 1 :: Float) / (fromIntegral l :: Float)
    knots = [delta * fromIntegral i | i <-  [0..(l-1)]] -- to handle overlapped interpolation range, assynmetry due to right-hand-endpoint
    rs = zip knots (tail knots)
    f r = [blossomUCB c params | params <- paramCombination n r]



findParameterUCB :: UCB -> Point -> Float -> [Parameter]
findParameterUCB c p r = concat [findParameterBezier b p r | b <- bs]
    where
    bs = ucb2bezier c



modifyWithPointUCB :: UCB -> Parameter -> Point -> UCB
modifyWithPointUCB c@(UCB ps n) t p = UCB ps' n
    where
    ps' = undefined



