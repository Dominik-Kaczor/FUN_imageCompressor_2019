--
-- EPITECH PROJECT, 2020
-- main
-- File description:
-- main
--

module Main where
import System.Environment
import System.IO.Error
import System.IO
import Control.Exception
import System.Random
import System.Exit
import Data.List
import Tools
import Core
import Error
import Parsing

imagecomp :: IO Int
imagecomp = do
            args <- getArgs
            case length args /= 3 of
                True -> exitWith (ExitFailure 84)
                False -> do
                        contents <- readFile (last args)
                        case contents == [] || error_core ((init args) ++ (words contents)) == 84 of 
                            True -> exitWith (ExitFailure 84)
                            False -> do
                                let nb_cluster = read (head args)
                                let converg = rDouble (head (tail args))
                                let point = reverse (get_point (fst (div_string_two (words contents) [] [])))
                                let color = reverse (get_color (snd (div_string_two (words contents) [] [])))
                                randI <- getStdRandom (randomR (0, (length color - 1)))
                                init_kmeans point color nb_cluster converg randI

main = do
    try imagecomp >>= mserror