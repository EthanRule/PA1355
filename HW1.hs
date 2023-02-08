-- CptS 355 - Spring 2023 -- Homework1 - Haskell
-- Name: Ethan Rule
-- Collaborators: Richard Castoro, Darrel Nitereka
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

make_sparse [] = []
make_sparse xs = makeSparseHelper xs 0
  where
    makeSparseHelper [] _ = []
    makeSparseHelper ((index, num) : xs) currentIndex
      | index == currentIndex = num : makeSparseHelper xs (currentIndex + 1)
      | index /= currentIndex = 0 : makeSparseHelper ((index, num) : xs) (currentIndex + 1)

-- P3(b) compress ; 15%

compress [] = []
compress (x:xs) = compressHelper (x:xs) 0
  where
    compressHelper [] _ = []
    compressHelper (x:xs) index
      | x == 0 = compressHelper xs (index + 1)
      | x /= 0 = (index, x) : compressHelper xs (index + 1)

-- P4 added_sums ; 8%
added_sums [] = []
added_sums (x:xs) = addHelper (x:xs) 0
  where
    addHelper [] _ = []
    addHelper (x:xs) num = x + num : addHelper xs (num + x)

-- P5 find_routes ; 8%

find_routes request [] = []
find_routes request ((route, dest):xs)
  | request `elem` dest = route : find_routes request xs
  | otherwise = find_routes request xs

-- P6 group_sum ; 15%

group_sum [] _ = []
group_sum (x:xs) mult = sumHelper (x:xs) [] mult
  where
    sumHelper [] [] mult = []
    sumHelper [] cList mult = [reverse cList] -- add any extra values to main list
    sumHelper (x:xs) cList mult
      | (x:xs) == [] = reverse cList : sumHelper xs [] mult -- add any extra values to main list
      | mult < sum (x:cList) = reverse cList : sumHelper (x:xs) [] (mult*2) -- add clist to main list
      | otherwise = sumHelper xs (x:cList) mult --add x to clist

-- Assignment rules ; 3%
-- Your own tests; please add your tests to the HW1Tests.hs file ; 6%
