{- Example of using the HUnit unit test framework.  See  http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "run" at the Haskell prompt.  -} 

module HW1Tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW1

-- P1(a) count tests 
counttest1 = TestCase (assertEqual "count-test1"
                                 2
                                 (count '1' "12345612") )
counttest2 = TestCase (assertEqual "count-test2"
                                 4
                                 (count '3' "123452334325612") )

-- P1(b) diff tests
difftest1 = TestCase (assertEqual "diff-test1"
                                 (sort [6,6,6,6,6,6,9])
                                 (sort $ diff [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,9,2] [1,2,3,4,5,7,7]) )
difftest2 = TestCase (assertEqual "diff-test2"
                                 [7,10]
                                 (sort $ diff [1,2,2,3,3,3,6,7,4,4,4,4,5,5,5,5,5,6,6,10] [1,1,2,2,3,3,4,4,5,6,6,6]) )


-- P1(c) bag_diff tests

bagdifftest1 = TestCase (assertEqual "bag_diff-test1"
                                 (sort [2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,9])
                                 (sort $ bag_diff [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,9,9,9] [1,2,3,4,5,7,7,9,9]) )
bagdifftest2 = TestCase (assertEqual "bag_diff-test2"
                                 (sort [3,4,4,5,5,5,5,7])
                                 (sort $ bag_diff [1,2,2,3,3,3,6,7,4,4,4,4,5,5,5,5,5,6,6,7,7,7] [1,1,2,2,3,3,4,4,5,6,6,6,7,7,7]) )

-- P2  everyN tests

p2_test1 = TestCase (assertEqual "everyN-test1"
                                  [4,8,12]
                                  (everyN [1,2,3,4,5,6,7,8,9,10,11,12,13] 4) )
p2_test2 = TestCase (assertEqual "everyN-test2"
                                  "STOP"
                                  (everyN "sStToOpP" 2))


-- P3(a) make_sparse tests

sparsetest1 = TestCase (assertEqual "make_sparse-test1"
                                  [0,0,0,30,0,0,0,0,0,0,100,110,0,0,0,20]
                                  (make_sparse [(3,30),(10,100),(11,110),(15,20)]) )
sparsetest2 = TestCase (assertEqual "make_sparse-test2"
                                  [0,1,2,0,4,0,6,0,0,9,1]
                                  (make_sparse [(1,1),(2,2),(4,4),(6,6),(9,9),(10,1)]) )


-- P3(b) compress tests

compresstest1 = TestCase (assertEqual "compress-test1"
                                  [(3,30),(10,100),(11,110),(15,2)]
                                  (compress [0,0,0,30,0,0,0,0,0,0,100,110,0,0,0,2]) )
compresstest2 = TestCase (assertEqual "compress-test2"
                                  [(1,1),(2,2),(4,4),(6,6),(9,9),(13,1)]
                                  (compress [0,1,2,0,4,0,6,0,0,9,0,0,0,1]) )

-- P4 added_sums tests

addedtest1 = TestCase (assertEqual "added_sums-test1"
                                  ([0,2,5,9,14,20,27,35,44,54])
                                  (added_sums [0,2,3,4,5,6,7,8,9,10]) )
addedtest2 = TestCase (assertEqual "added_sums-test2"
                                  ([1,3,6,10,15,21,28])
                                  (added_sums [1,2,3,4,5,6,7]) )

-- P5 find_routes tests

routes_test = [("Lentil",["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main", "Arbor", "Sunnyside", "Fountain", "Crestview", "Wheatland", "Walmart", "Bishop", "Derby", "Dilke"]), 
   ("Wheat",["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay", "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"]), 
   ("Silver",["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Outlet", "RockeyWay","Main"]),
   ("Blue",["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell", "Chinook", "Library"]),
   ("Gray",["TransferStation", "Wawawai", "Main", "Sunnyside","Crestview", "CityHall", "Stadium", "Colorado"]),
   ("Coffee",["TransferStation", "Grand", "Main", "Visitor","Stadium", "Spark", "CUB"])]    

routetest1 = TestCase (assertEqual "find_routes-test1"
                                  (sort ["Lentil","Gray","Silver","Coffee"])
                                  (sort $ find_routes "Main" routes_test ) )
routetest2 = TestCase (assertEqual "find_routes-test2"
                                  (sort ["Lentil", "Silver", "Gray", "Coffee"])
                                  (sort $ find_routes "Stadium" routes_test ) )


-- P6 group_sum tests

gsumtest1 = TestCase (assertEqual "(group_sum-test1)"
                                  [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17]]
                                  (group_sum [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17] 20) )
gsumtest2 = TestCase (assertEqual "(group_sum-test2)"
                                  [[12],[10,1,3,4,7,11],[22,2,5,40],[100,4]]
                                  (group_sum [12,10,1,3,4,7,11,22,2,5,40,100,4] 20) )


-- add the test cases you created to the below list. 
tests = TestList [ 
                   TestLabel "count test 1 " counttest1,
                   TestLabel "count test 2 " counttest2,
                   TestLabel "diff test 1 " difftest1,
                   TestLabel "diff test 2" difftest2,
                   TestLabel "bdiff test 1" bagdifftest1,
                   TestLabel "bdiff test 2 " bagdifftest2,
                   TestLabel "Problem 2- test1 " p2_test1,
                   TestLabel "Problem 2- test2 " p2_test2,
                   TestLabel "Problem 3a- test1 " sparsetest1,
                   TestLabel "Problem 3a- test2 " sparsetest2,
                   TestLabel "Problem 3b- test1 " compresstest1,
                   TestLabel "Problem 3b- test2 " compresstest2,
                   TestLabel "Problem 4- test1 " addedtest1,
                   TestLabel "Problem 4- test2 " addedtest2,
                   TestLabel "Problem 5- test1 " routetest1,
                   TestLabel "Problem 5- test2 " routetest2,
                   TestLabel "Problem 5- test1 " gsumtest1,
                   TestLabel "Problem 5- test2 " gsumtest2
                 ] 
                  
-- shortcut to run the tests
run = runTestTT  tests