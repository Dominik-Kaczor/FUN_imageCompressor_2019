-- 
-- EPITECH PROJECT, 2019
-- FUN_imageCompressor_2019
-- File description:
-- Cluster.hs
 --

module Cluster where
import Tools

init_cluster :: [(Int, Int, Int)] -> Int -> (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)]
init_cluster a 0 c d = d ++ [c]
init_cluster a b c d = init_cluster (remove c a) (b - 1) (find_best (remove c a) (average a 0 0 0 0)) (d ++ [c])

add_list_cluster :: Int -> [Int] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
add_list_cluster pos     []        []    clust = clust 
add_list_cluster pos (asig:ax) (list:lx) clust
    | length (asig:ax) == 0 || length (list:lx) == 0 = clust
    | asig == pos                                    = add_list_cluster pos ax lx clust ++ [list]
    | length (asig:ax) /= 0 || length (list:lx) /= 0 = add_list_cluster pos ax lx clust
    | otherwise                                      = clust

int_to_list_cluster :: Int -> [Int] -> [(Int, Int, Int)] -> [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
int_to_list_cluster (-1) list_int list asig = asig
int_to_list_cluster nb list_int list asig
    | nb >= 0    = int_to_list_cluster (nb - 1) list_int list asig ++ [add_list_cluster nb list_int list []]
    | otherwise  = asig

best_cluster :: (Int, Int, Int) -> [(Int, Int, Int)] -> Int -> Int -> Double -> Int
best_cluster color       []      best pos score = best
best_cluster color (cluster:clx) best pos score
    | distance color cluster < score            = best_cluster color clx pos  (pos + 1) (distance color cluster)
    | length (cluster:clx)   /= 0               = best_cluster color clx best (pos + 1) score
    | otherwise                                 = best

list_int_cluster :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [Int] -> [Int]
list_int_cluster     []      cluster listI = listI
list_int_cluster (color:clx) cluster listI
    | length (color:clx) > 0               = list_int_cluster clx cluster listI ++ [best_cluster color cluster 0 0 10000]
    | otherwise                            = listI

init_new_cluster :: [[(Int, Int, Int)]] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
init_new_cluster     []    cluster = cluster
init_new_cluster (list:lx) cluster
    | length (list:lx) > 0 = init_new_cluster lx (cluster ++ [average list 0 0 0 0]) 
    | otherwise            = cluster

