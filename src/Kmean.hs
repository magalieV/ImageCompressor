module Kmean
  (
    Cluster (Cluster, pixel_list, actual_color, last_color)
    , run_kmean
  ) where

import UtilsThings
import System.Random

data Cluster = Cluster { pixel_list :: [Pixel],
                         actual_color :: Color,
                         last_color :: Color,
                         id_cluster :: Int
                         } deriving (Eq)
instance Show Cluster where
    show (Cluster pi actual last_color id_clust) = "--\n" ++ show actual ++ "\n-\n" ++ show pi ++ "\n"

-- Init Clusters --
init_clusters :: Int -> [Pixel] -> [Cluster] -> StdGen -> [Cluster]
init_clusters 0 pixels clusters seed = clusters
init_clusters k pixels clusters seed = do
                                       let (a, b) = randomR(0, ((length pixels) - 1)) seed
                                       let col = color (pixels!!(a))
                                       init_clusters (k - 1) pixels (clusters ++ [(Cluster pixels col col) k]) b

-- set average --
set_average :: [Cluster] -> [Cluster] -> [Cluster]
set_average [] list = list
set_average clusters list = do
                            let actual_cluster = (head clusters)
                                average = average_of (pixel_list actual_cluster) 0 (Color 0 0 0)
                                tmp = Cluster (pixel_list actual_cluster) average (last_color actual_cluster) (id_cluster actual_cluster)
                            set_average (tail clusters) (list ++ [tmp])

-- run k-mean algo --
run_kmean :: Arguments -> [Pixel] -> IO ()
run_kmean args pixels = do
                       seed <- newStdGen
                       let clusters = init_clusters (number_of_color args) pixels [] seed
                           new_clusters = loop_kmean clusters pixels (converg args)
                       display_kmean new_clusters

-- test if all converge --
all_converg :: [Cluster] -> Float -> Bool
all_converg [] c = True
all_converg clusters c | value <= c = all_converg (tail clusters) c
                       | otherwise = False
                       where
                          actual_cluster = (head clusters)
                          value = distance (actual_color actual_cluster) (last_color actual_cluster)

-- loop kmean --
loop_kmean :: [Cluster] -> [Pixel] -> Float -> [Cluster]
loop_kmean clusters pixels c | (all_converg new_clusters c) == True = new_clusters
                             | otherwise = loop_kmean new_clusters pixels c
                             where
                                tmp = fill_clusters clusters [] pixels
                                new_clusters = set_average tmp []

-- calc_average_of_this --
average_of :: [Pixel] -> Float -> Color -> Color
average_of [] number_of_pixel new_pos | number_of_pixel == 0 = Color 0 0 0
                                      | otherwise = Color ((red new_pos) / number_of_pixel) ((green new_pos) / number_of_pixel) ((blue new_pos) / number_of_pixel)
average_of pixels number_of_pixel new_pos = do
                                            let color_pixel = (color (head pixels))
                                                pos = color_add new_pos color_pixel
                                            average_of (tail pixels) (number_of_pixel + 1) pos

-- create a list of closest pixel --
found_pixel_for_this_cluster :: Cluster -> [Cluster] -> [Pixel] -> [Pixel] -> [Pixel]
found_pixel_for_this_cluster cluster clusters [] my_list = my_list
found_pixel_for_this_cluster cluster clusters pixels my_list = do
                                                               let pix = (head pixels)
                                                                   is_closer = is_closest_to_other cluster clusters pix
                                                               if (is_closer == False) then found_pixel_for_this_cluster cluster clusters (tail pixels) (my_list ++ [pix])
                                                               else found_pixel_for_this_cluster cluster clusters (tail pixels) my_list


is_closest_to_other :: Cluster -> [Cluster] -> Pixel -> Bool
is_closest_to_other cluster [] pix = False
is_closest_to_other cluster clusters pix = do
                                           let dist = distance (actual_color cluster) (color pix)
                                               other_distance = distance (actual_color (head clusters)) (color pix)
                                           if (other_distance < dist && (id_cluster cluster) /= (id_cluster (head clusters))) then True
                                           else is_closest_to_other cluster (tail clusters) pix


-- Display Pixel --
display_pixel :: [Pixel] -> IO()
display_pixel [] = nop
display_pixel pixels = do
                       print (head pixels)
                       display_pixel (tail pixels)

-- Display k-mean --
display_kmean :: [Cluster] -> IO()
display_kmean [] = nop
display_kmean clusters = do
                         putStrLn("--")
                         print((actual_color (head clusters)))
                         putStrLn("-")
                         display_pixel (pixel_list (head clusters))
                         display_kmean (tail clusters)

-- fill the cluster --
fill_clusters :: [Cluster] -> [Cluster] -> [Pixel] -> [Cluster]
fill_clusters [] new_clusters [] = new_clusters
fill_clusters clusters new_clusters pixels = do
                                             let actual_cluster = (head clusters)
                                                 new_pixels_list = found_pixel_for_this_cluster actual_cluster clusters pixels []
                                                 average = (Color 0 0 0)
                                                 new_cluster = Cluster new_pixels_list average (actual_color actual_cluster) (id_cluster actual_cluster)
                                                 new_list = remove_list pixels new_pixels_list
                                             fill_clusters (tail clusters) (new_clusters ++ [new_cluster]) new_list
