{-# LANGUAGE FlexibleInstances,PackageImports #-}  
import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import "data-accessor" Data.Accessor
import Data.Colour.Names
import Data.Colour
getInteger :: String -> IO [Int]

getInteger str = do 
                    content <- readFile str
                    return $ map (\a -> (read a ::Int)) (filter (\a -> a /= "") $ lines content) 


response = ["fcfs_response.txt"]
turnaround = ["fcfs_turnaround.txt"]
waiting = ["fcfs_waiting.txt"]
appList [a] f = f a
appList2 [a,b] f = f a b
chart1 r s= layout
  where
    am :: Int -> Int
    am x = ((!!x) r)
    sm x = ((!!x) s)
    
    sinusoid1 = plot_lines_values ^= [[ (x,(am x)) | x <- [0..((length r)-1)]]]
              $ plot_lines_style  .> line_color ^= opaque blue
              $ plot_lines_title ^= "fcfs"
              $ defaultPlotLines
    sinusoid2 = plot_lines_values ^= [[ (x,(sm x)) | x <- [0..((length s)-1)]]]
              $ plot_lines_style  .> line_color ^= opaque red
              $ plot_lines_title ^= "sjfs"
              $ defaultPlotLines

    layout = layout1_title ^= "ResponseTime "
           $ layout1_plots ^= [Left (toPlot sinusoid1),Left (toPlot sinusoid2)]
           $ defaultLayout1

--x = plotWindow [1..4] [1,2,3,4]
main = do
        res <- sequence $ map getInteger response
        turn <- sequence $ map getInteger turnaround
        renderableToPDFFile (toRenderable (appList2 (res++turn) chart1)) 640 480 "test.pdf" 
        --plotWindow [1..(length $head res)] (head res)
        --print res
