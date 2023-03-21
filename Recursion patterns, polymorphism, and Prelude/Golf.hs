module Golf where

import Data.List

-- Exercise 1 - Hopscotch ... given a list, return a list of lists with every nth element of the originl list to be the
-- elements of the nth list
skips :: [a] -> [[a]]
skips xs = map (map (\(el,_) -> el)) skipped -- extract the elements from the (element, index) tuple
    where zipInner = zip xs [1..] -- append index to each element
          zipOuter = zip (replicate (length xs) zipInner) [1..] -- replicate to groups & append index to each group
          skipped = map (\(outEl, outIdx) -> filter (\(inEl, inIdx) -> inIdx `mod` outIdx == 0) outEl) zipOuter
          -- extract the indices of the group and elements and filter by their remainder

-- Exercise 2 - local maxima ... find all elements that are greater than the ones before and after it in the list
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | y > x && y > z = y : localMaxima (z:xs)
    | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

-- Exercise 3 - Histogram ... given a list of numbers from 0-9, create a histogram showing the frequency of occurrence
-- of each number
histogramRows :: [Integer] -> [String]
histogramRows [0,0,0,0,0,0,0,0,0,0] = []
histogramRows row = map (\x -> if x==0 then ' ' else '*') row : nextrow
    where nextrow = (histogramRows $ map (\x -> if x > 0 then (x-1) else 0) row)

histogram :: [Integer] -> String
histogram xs = unlines $ (reverse $ histogramRows frequencies) ++ ["==========","0123456789"]
    where frequencies = map (\n -> toInteger $ length $ findIndices (==n) xs) [0..9]

ioHistogram :: [Integer] -> IO ()
ioHistogram his = do putStr $ histogram his

