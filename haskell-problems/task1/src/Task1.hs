module Task1
    ( horse
    ) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M

sortOn :: Ord a1 => (a2 -> a1) -> [a2] -> [a2]
sortOn f = map snd . sortBy (comparing fst) . map (f &&& id)

clip :: (Ord a, Num a) => a -> a -> Bool
clip coord size = coord >= 0 && coord < size

valid :: (Ord a1, Num a1) => a1 -> M.Map (a1, a1) a2 -> (a1, a1) -> Bool
valid size solution xy@(x, y) = and [clip x size, clip y size, isNothing (M.lookup xy solution)]

neighbors :: Int -> M.Map (Int, Int) a2 -> (Int, Int) -> Int
neighbors size solution xy = length . filter (valid size solution) $ sequence moves xy

moves :: [(Int, Int) -> (Int, Int)]
moves = do
    f <- [(+), subtract]
    g <- [(+), subtract]
    (x, y) <- [(1, 2), (2, 1)]
    [f x *** g y]

solve :: Int -> M.Map (Int, Int) Int -> Int -> (Int, Int) -> [M.Map (Int, Int) Int]
solve size solution n xy = do
    guard (valid size solution xy)
    let solution'   = M.insert xy n solution
        sortedMoves = Task1.sortOn (neighbors size solution) (sequence moves xy)
    if n == size * size
        then [solution']
        else sortedMoves >>= solve size solution' (n+1)

printBoard :: Show a => Int -> M.Map (Int, Int) a -> String
printBoard size solution = board [0..size-1] where
    sqSize    = size * size
    elemSize  = length (show sqSize)
    separator = intercalate (replicate elemSize '-') (replicate (size + 1) "+")
    pad _ s   = replicate (elemSize - length s) ' ' ++ s
    elem xy   = pad elemSize . show $ solution M.! xy
    line y    = concat  . intersperseWrap "|" $ [elem (x, y) | x <- [0..size-1]]
    board     = unlines . intersperseWrap separator . map line
    intersperseWrap s ss = s : intersperse s ss ++ [s]

go :: Int -> (Int, Int) -> String
go size xy = case solve size M.empty 1 xy of
    []    -> "No solution found"
    (s:_) -> printBoard size s

horse :: (Int, Int) -> IO ()
horse xy = do
    putStrLn $ go 8 xy