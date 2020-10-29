module UtilsThings
  (
    nop
    , print_list
    , count_that
    , split_that
    , closest_color
    , remove_list
    , color_add
    , distance
    , Vector (Vector)
    , Point (Point, x_pos, y_pos)
    , Color (Color, red, green, blue)
    , Pixel (Pixel, point, color)
    , Arguments (Arguments, number_of_color, converg, path)
  ) where

-- Create differents type of variables --
data Vector = Vector Float Float Float deriving (Eq)
instance Show Vector where
    show (Vector x y z) = "Vector = " ++ show x ++ "," ++ show y ++ "," ++ show z
data Point = Point { x_pos :: Int,
                     y_pos :: Int
                     } deriving (Eq)
instance Show Point where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"
data Color = Color { red :: Float,
                     green :: Float,
                     blue :: Float
                     } deriving (Eq)
instance Show Color where
   show (Color r g b) = "(" ++ show (get_value_of_float r) ++ "," ++ show (get_value_of_float g) ++ "," ++ show (get_value_of_float b) ++ ")"
data Pixel = Pixel { point :: Point,
                     color :: Color
                    } deriving (Eq)
instance Show Pixel where
    show (Pixel point color) = show point ++ " " ++ show color
data Arguments = Arguments { number_of_color :: Int,
                             converg :: Float,
                             path :: String
                             } deriving (Eq)
instance Show Arguments where
    show (Arguments n e path) = "Args -> " ++ show n ++ "\t" ++ show e ++ "\t" ++ path

-- get round --
get_value_of_float :: Float -> Int
get_value_of_float number | number <= ((fromIntegral (truncate number) :: Float) + 0.5) = truncate number
                          | otherwise = round number

-- Print nothing on the console --
nop :: IO ()
nop = sequence_ []

-- Print a list (Debug) --
print_list :: [String] -> IO()
print_list [] = putStrLn("nothing more")
print_list list = do
                 putStrLn("new line : ")
                 putStrLn((head list))
                 print_list (tail list)

-- Count the number of a char in a string --
count_that :: String -> Char -> Int -> Int
count_that line letter counter | null line = counter
                               | (head line) == letter = do
                                                     let new_count = counter + 1
                                                     count_that (tail line) letter new_count
                               | otherwise = count_that (tail line) letter counter

-- Split a string into list --
split_that :: (Char -> Bool) -> String -> [String]
split_that p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split_that p s''
                            where (w, s'') = break p s'

-- Find absolute value --
absolute_value :: Int -> Int
absolute_value value | value < 0 = (value * (-1))
                     | otherwise = value

-- Calc the distance between two vector --
distance :: Color -> Color -> Float
distance u v = do
               let x = ((red u) - (red v)) ^ 2
                   y = ((green u) - (green v)) ^ 2
                   z = ((blue u) - (blue v)) ^ 2
               sqrt(x + y + z)

-- Fin closet color --
closest_color :: [Pixel] -> Color -> Color -> Color
closest_color [] color_to_found closest = closest
closest_color list color_to_found closest = do
                                   let col = color (head list)
                                       distance_col = distance color_to_found col
                                       distance_closest = distance color_to_found closest
                                   if (distance_col < distance_closest) then closest_color (tail list) color_to_found col
                                   else closest_color (tail list) color_to_found closest

-- remove a list from a list --
remove_list :: [Pixel] -> [Pixel] -> [Pixel]
remove_list list [] = list
remove_list list to_remove = do
                             let tmp = remove_this list (head to_remove) []
                             remove_list tmp (tail to_remove)

-- remove from list --
remove_this :: [Pixel] -> Pixel -> [Pixel] -> [Pixel]
remove_this [] pix new_list = new_list
remove_this pixels pix new_list | pix == (head pixels) = remove_this (tail pixels) pix new_list
                                | otherwise = remove_this (tail pixels) pix (new_list ++ [(head pixels)])

-- add color --
color_add :: Color -> Color -> Color
color_add a b = do
                let r = (red a) + (red b)
                    g = (green a) + (green b)
                    be = (blue a) + (blue b)
                Color r g be
