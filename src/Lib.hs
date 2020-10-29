module Lib
    ( someFunc
     , fill_info
     , create_list
    ) where
import Control.Exception
import System.Exit
import System.Environment

import UtilsThings
import SecurityFileContent
import PixelList
import ArgsManagement
import Kmean

-- Fill the list of pixel --
fill_info :: [String] -> [Pixel] -> IO()
fill_info [] map = putStrLn("nothing more")
fill_info str map = do
                    putStrLn("new line : ")
                    putStrLn((head str))
                    putStr("Counter for this line = ")
                    print(count_that (head str) ',' 0)
                    fill_info (tail str) map

-- Create the list info __
create_list :: [String] -> IO()
create_list [] = putStrLn("nothing more")
create_list info = fill_info info []

-- Clean map --
clean_map :: [String] -> [String]
clean_map [] = []
clean_map map = do
                let first_map = clean_info map [] ' '
                    second_map = clean_info first_map [] '('
                clean_info second_map [] ')'

-- Start algo --
start_compressor :: Arguments -> [String] -> IO ()
start_compressor args  map = do
                            let info = clean_map map
                                result = security_info info True
                            if (result == False) then exitWith(ExitFailure 84)
                            else do
                                --create_list info
                                 let pixel_map = create_pixel_list info []
                                 let result = security_pixel pixel_map
                                 if (result == False) then exitWith(ExitFailure 84)
                                 else do
                                      let col = closest_color pixel_map (Color 33 28 112) (color (head pixel_map))
                                      run_kmean args pixel_map

-- Run prog --
run_prog :: Arguments -> IO ()
run_prog args = do
                contents <- (readFile (path args)) `catch` readHandler
                open_file_security contents
                let map = lines contents
                    value = check_format map
                if (value == False) then exitWith(ExitFailure 84)
                else start_compressor args map

-- Main function for the moment --
someFunc :: IO ()
someFunc = do
           my_args <- getArgs
           let ac = (get_ac my_args 0)
           if (ac < 3 || ac > 3) then exitWith(ExitFailure 84)
           else do
                check_args my_args
                let number_color = read (head my_args) :: Int
                    map = (tail my_args)
                    convergence = read (head map) :: Float
                    map2 = (tail map)
                    args = Arguments number_color convergence (head map2)
                run_prog args

