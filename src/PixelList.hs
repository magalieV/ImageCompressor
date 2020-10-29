module PixelList
  (
  create_pixel_list
  , display_pixel_map
  , security_pixel
  ) where
import UtilsThings
import Data.Maybe
import Text.Read

display_pixel_map :: [Pixel] -> IO ()
display_pixel_map [] = nop
display_pixel_map pixel = do
                          print (head pixel)
                          display_pixel_map (tail pixel)

recover_point :: [String] -> Point
recover_point info = do
                     let x = readMaybe (info!!0) :: Maybe Int
                         y = readMaybe (info!!1) :: Maybe Int
                     if (isNothing x || isNothing y) then Point (-1) (-1)
                     else Point (fromJust x) (fromJust y)

get_point :: String -> Point
get_point info | null info = Point (-1) (-1)
               | otherwise = do
                            let info_split = split_that (==',') info
                            if (length info_split > 2 || length info_split < 2) then Point (-1) (-1)
                            else recover_point info_split

recover_color :: [String] -> Color
recover_color info = do
                     let r = readMaybe (info!!0) :: Maybe Int
                         g = readMaybe (info!!1) :: Maybe Int
                         b = readMaybe (info!!2) :: Maybe Int
                     if (isNothing r || isNothing g || isNothing b) then Color (-1) (-1) (-1)
                     else if ((fromJust r) < 0 || (fromJust g) < 0 || (fromJust b) < 0
                        || (fromJust r) > 255 || (fromJust g) > 255 || (fromJust b) > 255) then Color (-1) (-1) (-1)
                     else Color (fromIntegral (fromJust r) :: Float) (fromIntegral (fromJust g) :: Float) (fromIntegral (fromJust b) :: Float)

get_color :: String -> Color
get_color info | null info = Color (-1) (-1) (-1)
               | otherwise = do
                            let info_split = split_that (==',') info
                            if (length info_split > 3 || length info_split < 3) then Color (-1) (-1) (-1)
                            else recover_color info_split

create_pixel_list :: [String] -> [Pixel] -> [Pixel]
create_pixel_list [] pixel = pixel
create_pixel_list info pixel = do
                               let point = get_point (head info)
                                   map = tail info
                                   color = get_color (head map)
                               create_pixel_list (tail map) (pixel ++ [(Pixel point color)])

security_pixel :: [Pixel] -> Bool
security_pixel [] = True
security_pixel pixel = do
                       let col = color (head pixel)
                       let p = point (head pixel)
                       if (red col < 0 || green col < 0 || blue col < 0) then False
                       else if (x_pos p < 0 || y_pos p < 0) then False
                       else security_pixel (tail pixel)
