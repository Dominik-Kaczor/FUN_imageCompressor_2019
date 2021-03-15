--
-- EPITECH PROJECT, 2020
-- Tools
-- File description:
-- Tools of image compressor
--

module Tools where
import Data.Ord
import Data.List

remove element list = filter (\e -> e/=element) list

maxi xs = maximumBy (comparing fst) (zip xs [0..])

myNth :: [a] -> Int  -> a
myNth a b 
    | b < 0 || b >= length a = error("bad index")
    | b == 0                        = head a
    | otherwise                     = myNth (tail a) (b - 1)
    

rDouble :: String -> Double
rDouble = read

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
distance (r1, g1, b1) (r2, g2, b2) = sqrt (fromIntegral (((r1 - r2) ^ 2) + ((g1 - g2) ^ 2) + ((b1 - b2) ^ 2)))

distance_v :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance_v (x1, y1, z1) (x2, y2, z2) = sqrt (fromIntegral (x + y + z))
                                   where x = (x1 - x2) * (x1 - x2)
                                         y = (y1 - y2) * (y1 - y2)
                                         z = (z1 - z2) * (z1 - z2)

get_Distances :: (Int, Int, Int) -> [(Int, Int, Int)] -> [Float] -> [Float]
get_Distances (x1, y1, z1) [] a = a
get_Distances b c a = get_Distances b (tail c) (a ++ [(distance_v b (c !! 0))])

find_best :: [(Int, Int, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
find_best a b = a !! (snd (maxi (get_Distances b a [])))

average :: [(Int, Int, Int)] -> Int -> Int -> Int -> Int -> (Int, Int, Int)
average  []  x y z  0  = (-1, -1, -1)
average  []  x y z len = (x `div` len, y `div` len, z `div` len)
average ((a, b, c):as) x y z len
    | length ((a, b, c):as) > 0 = average as (x + a) (y + b) (z + c) (len + 1)
    | otherwise                 = (x `div` len, y `div` len, z `div` len)
