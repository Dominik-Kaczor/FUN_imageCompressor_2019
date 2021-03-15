-- 
-- EPITECH PROJECT, 2019
-- FUN_imageCompressor_2019
-- File description:
-- Display.hs
 --

module Display where
import Text.Printf

good_point :: [(Int, Int, Int)] -> [(Int, Int)] -> (Int, Int, Int) -> (Int, Int)
good_point (list:lx) (point:px) color  
    | list == color = point
    | otherwise     = good_point lx px color

display_color :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int)] -> IO Int
display_color    []     point color = return 0
display_color ((a, b, c):lx) point color
    | length ((a, b, c):lx) > 0 = do
                                printf "(%d,%d) (%d,%d,%d)\n" (fst (good_point color point (a, b, c))) (snd (good_point color point (a, b, c))) a b c
                                display_color lx point color
    | otherwise            = return 0

display_imageCompressor :: [(Int, Int, Int)] -> [[(Int, Int, Int)]] -> [(Int, Int)] -> [(Int, Int, Int)] -> IO Int
display_imageCompressor       []          []    point color = return 0
display_imageCompressor (cluster:clx) (list:lx) point color =   do
                                                                putStrLn "--"
                                                                print cluster 
                                                                putStrLn "-"
                                                                display_color list point color
                                                                display_imageCompressor clx lx point color
