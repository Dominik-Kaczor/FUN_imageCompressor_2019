--
-- EPITECH PROJECT, 2020
-- Error handling
-- File description:
-- Error handling
--

module Error where
import Control.Exception
import System.Environment
import System.IO.Error
import System.IO
import System.Exit
import Data.List
import Tools
import Parsing

checkInt :: [Char] -> Bool
checkInt x
    | all(>='0') x == True && all (<='9') x == True = True
    | otherwise                                     = False

checkNeg :: Char -> Bool
checkNeg x
    | x  == '-' = True
    | otherwise = False

readInt :: [Char] -> Maybe  Int
readInt [] = Nothing
readInt (a:b)
    | checkNeg      a       == True && checkInt b == True = Just (read(a:b))
    | checkInt      (a:b)   == True                       = Just (read(a:b))
    | otherwise                                           = Nothing

checkFloat :: [Char] -> Maybe Int -> Bool
checkFloat x (Just pos)
    | readInt   (take pos x) == Nothing       = False
    | readInt   (drop (pos + 1) x) == Nothing = False
    | otherwise                               = True

readFloat ::[Char] -> Maybe Int
readFloat [] = Nothing
readFloat x
    | elem      '.' x == False && readInt x /= Nothing                   = Just 0 
    | elem      '.' x == True  && checkFloat x (elemIndex '.' x) == True = Just 0
    | otherwise                                                          = Nothing

check_file_size :: [String] -> Bool
check_file_size x 
    | (length x) `mod` 2 == 0   = True 
    | otherwise                 = False

check_point :: String -> Bool
check_point str =  readPoint str

check_color :: String -> Bool
check_color str =  readColor str

readPoint :: String -> Bool
readPoint s = case reads s::[((Int, Int), String)] of
                [(x, "")] -> True
                _ -> False

readColor :: String -> Bool
readColor s = case reads s::[((Int, Int, Int), String)] of
                [(x, "")] -> True
                _ -> False

check_list_color :: [String] -> Bool
check_list_color [] = True
check_list_color (x:xs) 
    | (x:xs)        == []    = True
    | check_color x == False = False
    | otherwise              = check_list_color xs

check_list_point :: [String] -> Bool
check_list_point [] = True
check_list_point (x:xs) 
    | (x:xs)        == []    = True
    | check_point x == False = False
    | otherwise              = check_list_point xs

check_closteur :: [String] -> Int -> Bool
check_closteur x c
    | (length x) < c    = False
    | otherwise         = True

check_file :: [String] -> Int -> Int
check_file x c
    | check_file_size x                                 == False = 84
    | check_list_point (fst (div_string_two x [] []))   == False = 84
    | check_list_color (snd (div_string_two x [] []))   == False = 84
    | check_closteur   (snd (div_string_two x [] [])) c == False = 84
    | otherwise                                                  = 0

check_convergence :: String -> Int
check_convergence x
    | readFloat     x == Nothing = 84
    | rDouble       x <= 0       = 84
    | otherwise                  = 0

check_nb_color :: String -> Int
check_nb_color x
    | readInt       x == Nothing = 84
    | read          x <= 0       = 84
    | read          x > 16581375 = 84
    | otherwise                  = 0

error_core :: [String] -> Int
error_core (a:b:c)
    | check_nb_color        a          == 84 = 84
    | check_convergence     b          == 84 = 84
    | check_file            c (read a) == 84 = 84
    | otherwise                              = 0

mserror :: Either IOError a -> IO ()
mserror (Right _)    = return ()
mserror (Left error) = do
                        print("Bad using od the imageCompressor")
                        exitWith (ExitFailure 84)