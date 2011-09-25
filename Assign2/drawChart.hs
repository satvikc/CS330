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


response = ["1_fcfs_response.txt","1_sjf_response.txt","1_rr_response.txt","1_ps_response.txt","0_sjf_response.txt","0_ps_response.txt"]
turnaround = ["1_fcfs_turnaround.txt","1_sjf_turnaround.txt","1_rr_turnaround.txt","1_ps_turnaround.txt","0_sjf_turnaround.txt","0_ps_turnaround.txt"]
waiting = ["1_fcfs_waiting.txt","1_sjf_waiting.txt","1_rr_waiting.txt","1_ps_waiting.txt","0_sjf_waiting.txt","0_ps_waiting.txt"]
chart n title [(r1,r),(s1,s),(t1,t),(u1,u),(v1,v),(w1,w)]= layout
  where
    am :: Int ->[Int]-> Int
    am x y = ((!!x) y)
    
    sinusoid1 = plot_lines_values ^= [[ (x,(am x r)) | x <- [0..((length r)-1)]]]
              $ plot_lines_style  .> line_color ^= opaque blue
              $ plot_lines_title ^= r1
              $ defaultPlotLines
    sinusoid2 = plot_lines_values ^= [[ (x,(am x s)) | x <- [0..((length s)-1)]]]
              $ plot_lines_style  .> line_color ^= opaque red
              $ plot_lines_title ^= s1
              $ defaultPlotLines

    sinusoid3 = plot_lines_values ^= [[ (x,(am x t)) | x <- [0..((length t)-1)]]]
              $ plot_lines_style  .> line_color ^= opaque green
              $ plot_lines_title ^= t1
              $ defaultPlotLines
    sinusoid4 = plot_lines_values ^= [[ (x,(am x u)) | x <- [0..((length u)-1)]]]
              $ plot_lines_style  .> line_color ^= opaque purple
              $ plot_lines_title ^= u1
              $ defaultPlotLines
    sinusoid5 = plot_lines_values ^= [[ (x,(am x v)) | x <- [0..((length v)-1)]]]
              $ plot_lines_style  .> line_color ^= opaque navy
              $ plot_lines_title ^= v1
              $ defaultPlotLines
    sinusoid6 = plot_lines_values ^= [[ (x,(am x w)) | x <- [0..((length w)-1)]]]
              $ plot_lines_style  .> line_color ^= opaque magenta
              $ plot_lines_title ^= w1
              $ defaultPlotLines
    pl 1 = [Left (toPlot sinusoid1)] 
    pl 2 = (pl 1) ++  [Left (toPlot sinusoid2)]  
    pl 3 = (pl 2) ++  [Left (toPlot sinusoid3)]  
    pl 4 = (pl 3) ++  [Left (toPlot sinusoid4)]  
    pl 5 = (pl 4) ++  [Left (toPlot sinusoid5)]  
    pl 6 = (pl 5) ++  [Left (toPlot sinusoid6)]  
    layout = layout1_title ^= title
           $ layout1_plots ^= pl n
           $ defaultLayout1

--x = plotWindow [1..4] [1,2,3,4]
x1 = [1 ,2 ,3 ,4 ,5 ]
x2 = map (\x->x*x) x1
x3 = map (\x->x*x+2) x1
x4 = map (\x->x+x) x1
x5 = map (\x->x+x+x) x1
x6 = map (\x->x+x+x+x) x1

-- chart (arity maximum 6) "Chart Title" [(line 1 title, line 1 list) ,
-- (line 2 title,line 2 list) .....] always a list of six argument
-- expected . . So even if you want to plot 4 lines give 6 tuples .. can
-- repeat the last two beacuse they will be ignored suppose if you give
-- arity 4...
renderer criteria name = do
        [x1,x2,x3,x4,x5,x6] <- sequence $ map getInteger criteria
        --turn <- sequence $ map getInteger turnaround
        renderableToPDFFile (toRenderable (chart 6 name [("fcfs",x1),("nonpreemptive_sjf",x2),("rr",x3),("nonpreemptive_ps",x4),("preemptive_sjf",x5),("preemptive_ps",x6)])) 640 480 (name++".pdf") 
        --plotWindow [1..(length $head res)] (head res)
        --print response
main = do
        renderer response "response"
        renderer waiting "waiting"
        renderer turnaround "turnaround"

