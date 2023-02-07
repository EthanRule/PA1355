-- CptS 355 - Spring 2023 -- Homework1 - Haskell
-- Name:
-- Collaborators:
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module HW1 where

import Data.List
import System.Win32 (COORD (xPos))

-- P1(a) count ;  6%
count :: (Eq a) => a -> [a] -> Int
count _ [] = 0
count value (x : xs)
  | x == value = 1 + count value xs
  | otherwise = 0 + count value xs

-- P1(b) diff ;  6%
diff :: (Eq a) => [a] -> [a] -> [a]
diff [] [] = []
diff _ [] = []
diff [] _ = []
diff (y : ys) xs
  | y `elem` xs = diff ys xs
  | otherwise = y : diff ys xs

-- P1(c) bag_diff ; 8%

bag_diff [] ys = []
bag_diff (y : ys) xs
  | (count y ys - count y xs) >= 0 = y : bag_diff ys xs
  | otherwise = bag_diff ys xs

-- read the first value of ys
-- if y is in the list xs, remove it from xs
-- if y is not in xs, remove it from ys and add it to the return

-- P2  everyN ; 10%

everyN [] _ = []
everyN xs n = everyNHelper xs (n - 1)
  where
    y = n - 1
    everyNHelper [] _ = []
    everyNHelper (x : xs) n
      | n > 0 = everyNHelper xs (n - 1)
      | otherwise = x : everyNHelper xs y

-- P3(a) make_sparse ; 15%

-- make_sparse [] = []
-- make_sparse (x : xs) = makeSparseHelper xs x
--   where
--     len = length xs
--     makeSparseHelper [] _ = []
--     makeSparseHelper (x : xs) n
--       | fst n > 0 = 0 : makeSparseHelper xs (fst n - 1, snd n)
--       | otherwise = snd n : makeSparseHelper xs x
--         where
--           counter x y =

-- P3(b) compress ; 15%

-- P4 added_sums ; 8%

-- P5 find_routes ; 8%

-- P6 group_sum ; 15%

-- Assignment rules ; 3%
-- Your own tests; please add your tests to the HW1Tests.hs file ; 6%
