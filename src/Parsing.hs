--
-- EPITECH PROJECT, 2020
-- Parsing
-- File description:
-- Parsing file
--

module Parsing where

str_to_point :: String -> (Int, Int)
str_to_point = read

str_to_color :: String -> (Int, Int, Int)
str_to_color = read

get_color :: [String] -> [(Int, Int, Int)]
get_color [] = []
get_color (a:as)
    | length (a:as) > 0 = (str_to_color a):get_color as
    | otherwise         = []

get_point :: [String] -> [(Int, Int)]
get_point [] = []
get_point (a:as)
    | length (a:as) > 0 = (str_to_point a):get_point as
    | otherwise         = []

div_string_two :: [String] -> [String] -> [String] -> ([String], [String])
div_string_two [] b c = (b, c)
div_string_two (a:as:asx) b c
    | length (a:as:asx) > 0 = div_string_two asx (a:b) (as:c)
    | otherwise             = (b, c)