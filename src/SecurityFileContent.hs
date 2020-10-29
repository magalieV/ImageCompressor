module SecurityFileContent
  (
    check_if_counter_ok
    , security_info
    , clean_info
    , readHandler
    , open_file_security
    , check_format
  ) where
import UtilsThings
import System.Exit
import Data.Char

-- Check if there is a correct number of number --
-- True = last number equal 2, False = last number = 1 --
-- Check if there is all the time a point and a color --
check_if_counter_ok :: Int -> Bool -> Bool
check_if_counter_ok counter value | value == True && counter == 1 = True
                                  | value == False && counter == 2 = True
                                  | otherwise = False

-- Read the list to check if there is an error on the file --
security_info :: [String] -> Bool -> Bool
security_info [] value = value
security_info list value = do
                           let counter = count_that (head list) ',' 0
                               result = check_if_counter_ok counter value
                           if (value == True && result == True) then security_info (tail list) False
                           else if (result == True) then security_info (tail list) True
                           else False

-- Split the file for each ( or ) or space --
clean_info :: [String] -> [String] -> Char -> [String]
clean_info [] map char = map
clean_info info map char = do
                      let tmp = split_that(==char) (head info)
                      clean_info (tail info) (map ++ tmp) char

-- Catch an error when open a file and return a null string --
readHandler :: IOError -> IO String
readHandler e = do nop ; return ""

-- If the content of the file is null (can't be open or nothing ont the file) then return 84 --
open_file_security :: String -> IO()
open_file_security contents | null contents = exitWith(ExitFailure 84)
                            | otherwise = nop

-- Check if there is bad char (no good like number or comma --
search_bad_char :: String -> Bool
search_bad_char line | null line = False
                     | (head line) == '(' || (head line) == ')' = search_bad_char (tail line)
                     | (head line) == ' ' || (head line) == ',' = search_bad_char (tail line)
                     | (head line) == '+' || (head line) == '-' = search_bad_char (tail line)
                     | isDigit (head line) = search_bad_char (tail line)
                     | otherwise = True

-- Count the number of char on the line --
count_char_of_format :: [String] -> Bool
count_char_of_format [] = True
count_char_of_format lines = do
                             let count_space = count_that (head lines) ' ' 0
                                 count_parenthesis_open = count_that (head lines) '(' 0
                                 count_parenthesis_close = count_that (head lines) ')' 0
                                 count_comma = count_that (head lines) ',' 0
                             if (count_space == 1 && count_parenthesis_open == 2
                               && count_parenthesis_close == 2 && count_comma == 3) then check_format (tail lines)
                               else False

-- Check number of char in line --
check_format :: [String] -> Bool
check_format [] = True
check_format lines = do
                     let bad_char_found = search_bad_char (head lines)
                     if (bad_char_found == True) then False
                     else count_char_of_format lines

