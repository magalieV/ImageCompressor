module ArgsManagement
    (
    get_ac
    , check_args
    ) where
import System.Exit
import Data.Maybe
import Text.Read
import UtilsThings

get_ac :: [String] -> Int -> Int
get_ac [] ac = 0
get_ac str ac = do
                let value = ac + get_ac (tail str) ac
                value + 1

check_args :: [String] -> IO ()
check_args [] = usage >> exitWith(ExitFailure 84)
check_args info = do
                  let number_color = readMaybe (head info) :: Maybe Int
                      map = (tail info)
                      convergence = readMaybe (head (map)) :: Maybe Float
                  if (isNothing number_color) then usage >> exitWith(ExitFailure 84)
                  else if (isNothing convergence) then usage >> exitWith(ExitFailure 84)
                  else if (fromJust number_color < 1) then usage >> exitWith(ExitFailure 84)
                  else nop

usage :: IO ()
usage = do
        putStrLn "USAGE: ./imageCompressor n e IN\n"
        putStrLn "\tn\tnumber of colors in the final image"
        putStrLn "\te\tconvergence limit"
        putStrLn "\tIN\tpath to the file containing the colors of the pixels"
