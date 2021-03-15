-- 
-- EPITECH PROJECT, 2019
-- FUN_imageCompressor_2019
-- File description:
-- test.hs
 --

import Test.HUnit
import Tools
import Cluster

tesval = [(66,20,26), (98, 99, 233), (45, 12, 167), (33, 16, 94)]

findBest = TestCase $ assertEqual "Basic find_best" (98, 99, 233) (find_best tesval (0, 0, 0))

averageUse = TestCase $ assertEqual "Normal average use" (2, 2, 2) (average [(1, 1, 1), (3, 3, 3)] 0 0 0 0)

distanceUse = TestCase $ assertEqual "Normal distance use" 3.7416573867739413 (distance (1, 2, 3) (0, 0, 0))

distance_vUse = TestCase $ assertEqual "Normal distance use" 3.7416573867739413 (distance_v (1, 2, 3) (0, 0, 0))

getDistances = TestCase $ assertEqual "Normal use of get_Distances" [71.260086, 267.80215, 170.09409, 97.473076] (get_Distances (1, 2, 3) tesval [])

my_nth = TestCase $ assertEqual "Normal use of myNth" 3 (myNth [1, 2, 3] 2)

my_nth_head = TestCase $ assertEqual "head index" 1 (myNth [1, 2, 3] 0)

list_int_clusters = TestCase $ assertEqual "list of int for clusters" [3, 2, 1, 0] (list_int_cluster tesval tesval [])

best_clusters = TestCase $ assertEqual "find the best cluster" 3 (best_cluster (1, 2, 3) tesval 3 3 37.6)

init_clusters = TestCase $ assertEqual "initialise cluster" [(1, 2, 3), (98, 99, 233), (66, 20, 26)] (init_cluster tesval 2 (1, 2, 3) [])

add_list_clusters = TestCase $ assertEqual "add_list_cluster" [(45, 12, 167)] (add_list_cluster 1 [3, 2, 1, 0] tesval [])

testlist = TestList [TestLabel "findBest" findBest,
                     TestLabel "average" averageUse,
                     TestLabel "distance_v" distance_vUse,
                     TestLabel "getDistances" getDistances,
                     TestLabel "myNth" my_nth,
                     TestLabel "myNth_head" my_nth_head,
                     TestLabel "list_int_cluster" list_int_clusters,
                     TestLabel "best_cluster" best_clusters,
                     TestLabel "init_cluster" init_clusters,
                     TestLabel "add_list_cluster" add_list_clusters,
                     TestLabel "distance" distanceUse]

main :: IO ()
main = do
  runTestTT testlist
  return ()
