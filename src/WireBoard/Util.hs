module WireBoard.Util
    ( assert
    , assertM
    , validIdx
    , extract
    )
where

------------------------------
-- Util
------------------------------

assert :: (Show s) => Bool -> s -> a -> a
assert b s = if b then id else error (show s)


assertM :: (Show s, Monad m) => Bool -> s -> m ()
assertM b s = if b then return () else error (show s)


validIdx :: [a] -> Int -> Bool
validIdx as i = 0 <= i && i < length as


extract :: Int -> Int -> [a] -> [a]
extract i j xs = take j $ drop i xs

