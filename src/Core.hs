--
-- EPITECH PROJECT, 2020
-- image_compresor
-- File description:
-- Core
--

module Core where
import Display
import Tools
import Cluster

assignation :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [[(Int, Int, Int)]]
assignation nb list cluster = int_to_list_cluster (nb - 1) (reverse (list_int_cluster list cluster [])) list []

get_new_converge :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Double -> Double
get_new_converge    []        []    big = big
get_new_converge (new:nx) (last:lx) big
    | distance new last > big                     = get_new_converge nx lx (distance new last)
    | length (new:nx) > 0 && length (last:lx) > 0 = get_new_converge nx lx big
    | otherwise                                   = big

kmeans :: [(Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [[(Int, Int, Int)]] -> Double -> Double -> IO Int
kmeans point cluster color liste converge last
    | last <= converge = display_imageCompressor cluster liste point color
    | otherwise        = kmeans point (init_new_cluster liste []) color (assignation (length cluster) color (init_new_cluster liste [])) converge (get_new_converge cluster (init_new_cluster liste []) 0)

init_kmeans :: [(Int, Int)] -> [(Int, Int, Int)] -> Int -> Double -> Int -> IO Int
init_kmeans point color nb converge rand = kmeans point (init_cluster (remove (myNth color rand) color) (nb - 1) (myNth color rand) []) color (assignation nb color (init_cluster (remove (myNth color rand) color) (nb - 1) (myNth color rand) [])) converge (converge + 1)
