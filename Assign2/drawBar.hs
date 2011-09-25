module DrawBar where 

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Environment(getArgs)

getInteger :: String -> IO [Double]

getInteger str = do 
                    content <- readFile str
                    return $ map (\a -> (read a ::Double)) (filter (\a -> a /= "") $ lines content) 
avg = ["1_fcfs_avg.txt","1_sjf_avg.txt","1_rr_avg.txt","1_ps_avg.txt","0_sjf_avg.txt","0_ps_avg.txt"]


chart lo res wait turn = toRenderable layout
 where
  layout = 
        layout1_title ^= "Legend Test"
      $ layout1_title_style ^: font_size ^= 10
      $ layout1_bottom_axis ^: laxis_generate ^= autoIndexAxis alabels
      $ layout1_left_axis ^: laxis_override ^= (axisGridHide.axisTicksHide)
      $ layout1_plots ^= [ Left (plotBars bars2) ]
      $ layout1_legend ^= Just lstyle
      $ defaultLayout1 :: Layout1 PlotIndex Double

  bars2 = plot_bars_titles ^= ["fcfs","nonp_sjf","rr","nonp_ps","preemptive_sjf","preemptive_ps"]
      $ plot_bars_values ^= addIndexes [res,
                                       wait, 
                                       turn]
      $ plot_bars_style ^= BarsClustered
--      $ plot_bars_spacing ^= BarsFixWidth 60
      $ plot_bars_item_styles ^= map mkstyle (cycle defaultColorSeq)
      $ defaultPlotBars

  alabels = [ "Response","Waiting","tunraround" ]

  lstyle = legend_orientation ^= lo
         $ defaultLegendStyle

  btitle = ""
  mkstyle c = (solidFillStyle c, Nothing)

--main1 :: [String] -> IO (PickFn ())
--main1 ["small"]  = renderableToPNGFile (chart (LORows 3)) 320 240 "test15_small.png"
--main1 ["big"]    = renderableToPNGFile (chart (LORows 3)) 800 600 "test15_big.png"
--main1 _          = renderableToWindow  (chart (LORows 3) ) 640 480 >> return undefined

main = do 
        xs <- sequence $ map getInteger avg
        let res = map head xs
        let wait = map (head.tail) xs
        let turn = map last xs
        renderableToPDFFile (chart (LORows 3) res wait turn ) 640 480 "averages.pdf"



