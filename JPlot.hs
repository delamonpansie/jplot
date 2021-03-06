{-# LANGUAGE OverloadedStrings #-}

{-
   Copyright (C) 2012 Yuriy Vostrikov

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
   OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
   SUCH DAMAGE.
-}


import System.IO (openFile, IOMode(..), hFileSize, hClose)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), dropExtension)

import Data.Array.Unboxed
import Data.Maybe (isJust, fromJust)
import Data.List (sort, isSuffixOf, sortBy, nubBy, maximumBy)
import Data.Ord (comparing)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lex.Double as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Colour.Names
import Data.Colour.SRGB (RGB(..), toSRGB)
import qualified Data.Function as F

import Text.Printf (printf)
import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M

import Control.Applicative ((<$>))
import Control.Monad (filterM, guard, liftM, zipWithM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)

import Debug.Trace (trace)

getFileSize :: FilePath -> IO Integer
getFileSize path = bracket (openFile path ReadMode) hClose (liftM fromIntegral . hFileSize)

rcomparing :: Ord b => (a -> b) -> a -> a -> Ordering
rcomparing f a b = f b `compare` f a

defaultDir = "."

axisLabelSize = 10
axisHeight = axisLabelSize * 3.3

data JLogGraph = JLogGraph { jLogGraphN :: Int,
                             jLogGraphTitle :: String,
                             jLogGraphUnit :: String,
                             jLogGraphSymbol :: String,
                             jLogGraphFactor :: Double } deriving (Show)

jLog2Graphs = [ JLogGraph 1 "U-BEC" "V" "Ubec" 0.1,
                JLogGraph 2 "I-BEC" "A" "Ibec" 0.1,
                JLogGraph 3 "U-BAT" "V" "Ubat" 0.1,
                JLogGraph 4 "I-Motor" "A" "Imot" 0.1,
                JLogGraph 5 "I-Motor/Int" "A" "ImotI" 0.1,
                JLogGraph 6 "Throttle" "%" "TH" 1.0,
                JLogGraph 7 "PWM-Motor" "%" "PWMmot" 1.0,
                JLogGraph 8 "RPM-Uni" "RPM" "RPM-U" 10,
                JLogGraph 9 "RPM-Motor" "RPM" "RPM-M" 10,
                JLogGraph 10 "Capacity" "mAh" "CAP" 1.0,
                JLogGraph 11 "Temp-PA" "C" "T-PA" 1.0,
                JLogGraph 12 "Temp-BEC" "C" "T-BEC" 1.0,
                JLogGraph 13 "Power" "W" "PWR" 1.0,
                JLogGraph 14 "Power/Int" "W" "PwrI" 1.0,
                JLogGraph 15 "IbecMax" "A" "IbecMax" 0.1,
                JLogGraph 16 "ImotMax" "A" "ImotMax" 0.1,
                JLogGraph 17 "ALARM: Capacity" "Sig" "A:Cap" 1,
                JLogGraph 18 "ALARM: Ubat" "Sig" "A:Ubat" 1,
                JLogGraph 19 "ALARM: TempPA" "Sig" "A:Tpa" 1,
                JLogGraph 20 "ALARM: UbecDrop" "Sig" "A:UbecDrop" 1,
                JLogGraph 21 "ALARM: EXT:Temp1" "Sig" "A:ExtTemp1" 1,
                JLogGraph 22 "ALARM: EXT:Temp2" "Sig" "A:ExtTemp2" 1,
                JLogGraph 23 "ALARM: EXT:Temp3" "Sig" "A:ExtTemp3" 1,
                JLogGraph 24 "ALARM: EXT:Temp4" "Sig" "A:ExtTemp4" 1,
                JLogGraph 25 "ALARM: EXT:Temp5" "Sig" "A:ExtTemp5" 1,
                JLogGraph 26 "EXT:Temp1" "C" "EXT:T1" 0.1,
                JLogGraph 27 "EXT:Temp2" "C" "EXT:T2" 0.1,
                JLogGraph 28 "EXT:Temp3" "C" "EXT:T3" 0.1,
                JLogGraph 29 "EXT:Temp4" "C" "EXT:T4" 0.1,
                JLogGraph 30 "EXT:Temp5" "C" "EXT:T5" 0.1,
                JLogGraph 31 "EXT:RPM" "RPM" "EXT:R" 1]

kosmikGraphs = [ JLogGraph 1 "RPMmot_2pn" "RPM" "RPM" 1.0,
                 JLogGraph 2 "Ubat" "V" "Ubat" 1.0,
                 JLogGraph 3 "Imot(integrated)" "A" "Imot(integrated)" 1.0,
                 JLogGraph 4 "Ah" "Ah" "Ah" 1.0,
                 JLogGraph 5 "Imot" "A" "Imot" 1.0,
                 JLogGraph 6 "tFET" "°C" "tFET" 1.0,
                 JLogGraph 7 "PWM" "%" "PWM" 1.0,
                 JLogGraph 8 "Ubec" "V" "Ubec" 1.0,
                 JLogGraph 9 "Ibec" "A" "Ibec" 1.0,
                 JLogGraph 10 "tBEC" "°C" "tBEC" 1.0,
                 JLogGraph 11 "throttle input" "%" "thr-in" 1.0,
                 JLogGraph 12 "throttle effective" "%" "throttle-eff" 1.0,
                 JLogGraph 13 "effective timing" "°" "tmg" 1 ]


jLog2GraphsToggled = map (\x -> (x, jLogGraphN x `elem` [3, 4, 7, 8], Nothing, [])) jLog2Graphs
kosmikGraphsToggled = map (\x -> (x, jLogGraphN x `elem` [2, 5, 7, 1], Nothing, [])) kosmikGraphs

colors = cycle $ [darkgreen, darkred,
                  green, red, blue,
                  orange, cyan, crimson, green, yellow, magenta,
                  violet, pink, gold, purple, royalblue,
                  darkcyan, darkblue, darkmagenta,
                  darkorange, darkviolet, olive]


data Matrix = Matrix Int (UArray (Int,Int) Double) deriving (Show)
matrixNew :: [[Double]] -> Matrix
matrixNew ps = Matrix rowCount (listArray ((0,0), (rowCount,rowLen)) $ concat ps)
                 where rowLen = (length $ ps !! 0) - 1
                       rowCount = length ps
matrixCol (Matrix rowCount x) n = [x ! (i,n) | i <- [0..rowCount - 1]]

parseMatrix :: B.ByteString -> Maybe Matrix
parseMatrix text = do
  let raw = map (map readDouble . drop 2 . B.split ';')
            $ filter (\line -> B.length line > 0 && B.head line == '$')
            $ B.splitWith (\b -> b == '\r' || b == '\n') text
  guard $ length raw > 1
  let rowLen = length $ head raw
  guard $ rowLen > 2
  let xs = filter (\row -> length row == rowLen && head row > 0) raw
  guard $ length xs > 2
  return $ matrixNew xs
   where readDouble str = case B.readDouble str of
                            Just (x, _) -> x
                            Nothing -> 0

data JLogFile = JLogFile { jLogFileName :: FilePath,
                           jLogDataMatrix :: Maybe Matrix } deriving (Show)

jLogFile :: FilePath -> FilePath -> IO JLogFile
jLogFile dirName fileName = do
  fileContents <- B.readFile $ dirName </> fileName
  return $ JLogFile (dropExtension fileName) (parseMatrix fileContents)


jLogDir :: FilePath -> String -> IO [JLogFile]
jLogDir dirName extension =
  fileList dirName >>=
  filterM bigEnough >>=
  mapM (jLogFile dirName)
    where
      bigEnough file = do size <- getFileSize $ dirName </> file
                          return $ size > 1024
      fileList dirName = reverse . sort . filter (isSuffixOf extension) <$>
                         getDirectoryContents dirName

data Dim = Dim { width :: Double,
                 height :: Double }

data Plot = Plot { plotG :: JLogGraph,
                   plotS :: Maybe M.Matrix,
                   plotT :: [Double],
                   plotV :: [Double],
                   plotMaxT :: Double,
                   plotMaxV :: Double }

mkPlot g scaler (JLogFile _ Nothing) = Plot g Nothing [] [] 0 0
mkPlot g scaler (JLogFile _ (Just m)) = Plot g scaler ts vs maxT maxV
    where ts = matrixCol m 0
          vs = matrixCol m (jLogGraphN g)
          (maxT, maxV) = (maximum ts, maximum vs)

plotDim (Plot _ _ _ _ maxT maxV) = Dim maxT maxV
plotN = jLogGraphN . plotG
plotTitle = jLogGraphTitle . plotG
plotFactor = jLogGraphFactor . plotG

plotColor p =
    case lookup (plotN p) colorMap of
      Just color -> toSRGB color
      Nothing -> toSRGB black
    where colorMap = zip [1..] colors


scale factor xs = map (\e -> (e - minX) / (maxX - minX) * factor) xs
    where maxX = maximum xs
          minX = minimum xs

data LabelPlace = LabelUp | LabelDown deriving (Eq)

renderAxis :: String -> LabelPlace -> Double -> [Double] -> C.Render ()
renderAxis title place width rs = do
  C.selectFontFace "Sans" C.FontSlantNormal C.FontWeightNormal
  C.setFontSize axisLabelSize
 
  let 
  ruler
  mapM_ tick rxs
  label place 2.5 (title, 30)
  C.setSourceRGB 0 0 0
  mapM_ (label place 1) rxs
 
   where rxs = zip (map (show . round) rs) (scale width rs)
 
         tickHeight = axisHeight * 0.15
         labelOffset = tickHeight * 1.05
         
         ruler = 
             do C.moveTo 0 0
                C.lineTo width 0
                C.setLineWidth 0.9
                C.stroke

         tick (_, x) = 
             do C.moveTo x (-tickHeight / 2)
                C.lineTo x (tickHeight / 2)
                C.setLineWidth 2.2
                C.stroke

         label place factor (str, x) = 
             do exts <- C.textExtents str
                let (sx, sy) = (C.textExtentsWidth exts, C.textExtentsHeight exts)
                C.moveTo (x - sx / 2) (if place == LabelUp 
                                       then (labelOffset * factor) + sy 
                                       else -labelOffset * factor)
                C.showText str
                

roundPoints :: Double -> Double -> [Double]
roundPoints minX maxX =
    snd $
    head $ 
    sortBy (comparing fst) $
    map (\x -> (abs $ 15 - length x, x)) $
    map points [(1,2), (2,2), (5,2), (1,1)]
        where
          log10 x = round $ logBase 10 x
          step mul dec = (mul * 10) ^^ (log10 (maxX - minX) - dec)
          points (mul, dec) = [minX, minX + (step mul dec) .. maxX]


plotScaler :: CanvasLayout -> Double -> Double -> Maybe M.Matrix
plotScaler _ _ 0 = Nothing
plotScaler _ 0 _ = Nothing
plotScaler (Layout top main tAxis yAxis) maxT maxV = Just $
  M.translate (width yAxis) (height top) $
  M.translate 0 (height main) $
  M.scale (width main / maxT) (height main / maxV) $
  flipY
  where
    flipY = M.Matrix 1 0 0 (-1) 0 0
    
renderPlot :: Plot -> C.Render ()
renderPlot (Plot _ Nothing _ _ _ _) = return ()
renderPlot p@(Plot g (Just scaler) ts vs _ _) = do
  C.save
  C.setMatrix scaler
  C.moveTo (head ts) (head vs)
  zipWithM_ C.lineTo (tail ts) (tail vs)
  C.restore
  C.setLineWidth 0.8
  let RGB r g b = plotColor p
  C.setSourceRGB r g b
  C.stroke

data CanvasLayout = Layout Dim Dim Dim Dim
layout :: Dim -> Int -> CanvasLayout
layout canvas graphsCount = Layout top main tAxis yAxis
  where
      top = Dim (width canvas) axisHeight
      main = Dim (width canvas - width yAxis) (height canvas - height tAxis - height top)
      tAxis = Dim (width main) axisHeight
      yAxis = Dim (axisHeight * fromIntegral graphsCount) (height main)

renderAxisAndPlots :: CanvasLayout -> [Plot] -> C.Render ()
renderAxisAndPlots (Layout top main tAxis yAxis) [] = return ()
renderAxisAndPlots (Layout top main tAxis yAxis) plots = do
  let renderXAxis p = do 
        C.save
        C.translate (width yAxis) (height main + height top)
        renderAxis "" LabelUp (width tAxis) [0,60..plotMaxT p]
        C.restore

      renderYAxis offset p = do
        C.save
        C.translate offset (height main + height top)
        C.rotate (-pi / 2)
        let RGB r g b = plotColor p
            maxV = plotFactor p * plotMaxV p
            points = if maxV > 0
                     then roundPoints 0 maxV
                     else [0, 1]
        C.setSourceRGB r g b
        renderAxis (plotTitle p) LabelDown (height yAxis) points
        C.restore

      longest = head $ sortBy (rcomparing plotMaxT) plots
      largest = sortBy (comparing plotN) $
                nubBy ((==) `F.on` plotN) $
                sortBy (rcomparing plotMaxV) plots

  renderXAxis longest
  mapM_ (uncurry renderYAxis) $ zip [axisHeight, axisHeight * 2 ..] largest
  mapM_ renderPlot plots


type FileStore = ListStore (JLogFile, Bool)
type GraphStore = ListStore (JLogGraph, Bool, Maybe M.Matrix, [Plot])

updateStores :: Dim -> FileStore -> GraphStore -> IO (CanvasLayout, [Plot])
updateStores dim fileStore graphStore = do
  graphsCount <- length . filter (\(_,b,_,_) -> b) <$> listStoreToList graphStore
  let l = layout dim graphsCount
  files <- (map fst . filter snd) <$> listStoreToList fileStore
  treeModelForeach graphStore $ \iter -> 
      do let i = listStoreIterToIndex iter
         val <- listStoreGetValue graphStore i
         listStoreSetValue graphStore i (update l files val)
         return False
  plots <- concatMap (\(_,_,_,v) -> v) <$> listStoreToList graphStore
  return (l, plots)
   where
     update _ [] (g, b, _, _) = (g, b, Nothing, [])
     update layout _ (g, False, _, _) = (g, False, Nothing, [])
     update layout files (g, True, _, _) = (g, True, scaler, ps)
       where ps = map (mkPlot g scaler) files
             maxT = maximum $ map plotMaxT ps
             maxV = maximum $ map plotMaxV ps
             scaler = plotScaler layout maxT maxV


packLabelCell :: (a -> String) -> ListStore a -> TreeViewColumn -> IO CellRendererText
packLabelCell unpack store col = do
  cell <- cellRendererTextNew
  cellLayoutPackStart col cell True
  cellLayoutSetAttributes col cell store $ \val -> [ cellText := unpack val ]
  return $ cell

packToggleCell :: (a -> Bool) -> (a -> a) -> ListStore a -> TreeViewColumn -> IO CellRendererToggle
packToggleCell unpack flip store col = do
  cell <- cellRendererToggleNew
  cellLayoutPackStart col cell True
  cellLayoutSetAttributes col cell store $ \val -> [ cellToggleActive := unpack val ]
  cell `on` cellToggled $ \path ->
      do let i = read path
         val <- listStoreGetValue store i
         listStoreSetValue store i (flip val)  
  return $ cell

packDescrCell view store f = do
  col <- treeViewColumnNew
  treeViewAppendColumn view col
  cell <- cellRendererTextNew
  cell `set` [ cellTextFont := "Sans 9", cellText := "\t" ]
  cellLayoutPackStart col cell True
  cellLayoutSetAttributeFunc col cell store $ \iter -> do
      let i = (listStoreIterToIndex iter)
      val <- listStoreGetValue store i
      text <- f val
      cell `set` [ cellText := text ]

  
toggleViewNew store = do
  window <- scrolledWindowNew Nothing Nothing
  view <- treeViewNewWithModel store
  window `set` [ containerChild := view,
                 scrolledWindowHscrollbarPolicy := PolicyNever,
                 scrolledWindowVscrollbarPolicy := PolicyAutomatic ]
  treeViewSetHeadersVisible view False
  col <- treeViewColumnNew
  treeViewAppendColumn view col
  return (window, view, col)

widgetInvalidate w offset = do
  realized <- widgetGetRealized w
  when realized $ do
    dw <- widgetGetDrawWindow w
    Rectangle _ _ width height <- widgetGetAllocation w
    drawWindowInvalidateRect dw (Rectangle offset 0 (width - offset) height) True

type Canvas = DrawingArea
canvasNew :: IO (Frame, Canvas)
canvasNew = do
  canvas <- drawingAreaNew
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
  canvasContainer <- frameNew
  containerAdd canvasContainer canvas
  return (canvasContainer, canvas)

canvasRender :: Canvas -> C.Render () -> IO ()
canvasRender canvas renderer = do
  dw <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  drawWindowBeginPaintRect dw (Rectangle 0 0 width height)
  renderWithDrawable dw renderer
  drawWindowEndPaint dw


main = do
  initGUI

  mousePos <- newIORef (0, 0)
  dirRef <- newIORef defaultDir
  fileStore <- listStoreNew []
  graphStore <- listStoreNew jLog2GraphsToggled

  jlog <- radioButtonNewWithLabel "JLog"
  kosmik <- radioButtonNewWithLabelFromWidget jlog "Kosmik"
  radioBox <- hBoxNew False 0
  boxPackStart radioBox jlog PackGrow 0
  boxPackStart radioBox kosmik PackGrow 0

  (canvasContainer, canvas) <- canvasNew
  canvas `on` exposeEvent $ do
         liftIO $ do
           (width, height) <- widgetGetSize canvas
           let dim = Dim (fromIntegral width) (fromIntegral height)
           (layout, plots) <- updateStores dim fileStore graphStore
           canvasRender canvas $ renderAxisAndPlots layout plots
         return True

  (filesContainer, _, filesCol) <- toggleViewNew fileStore
  packLabelCell (jLogFileName . fst) fileStore filesCol
  filesToggle <- packToggleCell snd (\(a,b) -> (a,not b)) fileStore filesCol
  
  (plotsContainer, plotsView, plotsCol) <- toggleViewNew graphStore 
  graphLabel <- packLabelCell (\(g,_,_,_) -> jLogGraphTitle g) graphStore plotsCol
  graphToggle <- packToggleCell (\(_,b,_,_) -> b) (\(a,b,c,d) -> (a,not b,c,d)) graphStore plotsCol

  mapM_ (\obj -> obj `on` cellToggled $ \_ -> widgetInvalidate canvas 0) 
        [filesToggle, graphToggle]

  packDescrCell plotsView graphStore $ \val -> do
      realized <- widgetGetRealized canvas
      if not realized
      then return "\t"
      else case val of
             (_, False, _, _) -> return "\t"
             (_, _, Nothing, _) -> return "\t"
             (_, _, _, []) -> return "\t"
             (g, _, scaler, ps) -> do
                 (mX, mY) <- readIORef mousePos
                 dw <- widgetGetDrawWindow canvas
                 renderWithDrawable dw $ do
                        C.setMatrix $ fromJust scaler
                        (_, y) <- C.deviceToUser mX mY
                        return $ printf "%.1f" $ jLogGraphFactor g * y

  widgetAddEvents canvas [PointerMotionMask]
  canvas `on` motionNotifyEvent $ tryEvent $ do 
      c <- eventCoordinates
      liftIO $ do writeIORef mousePos c
                  -- invalidate only column with mouse pos
                  mouseColOfft <- treeViewColumnGetWidth plotsCol
                  widgetInvalidate plotsView mouseColOfft

  let reloadDir = do
         dir <- readIORef dirRef
         isJlog <- toggleButtonGetActive jlog
         files <- jLogDir dir (if isJlog then "txt" else "dat")
         listStoreClear fileStore
         mapM_ (listStoreAppend fileStore) $ zip files (True : cycle [False])
         widgetInvalidate canvas 0
  reloadDir

  jlog `on` toggled $ do
     isJlog <- toggleButtonGetActive jlog
     listStoreClear graphStore
     mapM_ (listStoreAppend graphStore) (if isJlog
                                         then jLog2GraphsToggled
                                         else kosmikGraphsToggled)
     reloadDir

  hbox <- hBoxNew False 0
  vbox <- vBoxNew False 0

  dirChooser <- buttonNewWithLabel "Select Log Folder"
  dirChooser `onClicked` do
    fcd <- fileChooserDialogNew (Just "Select Log Folder") Nothing
    	   			FileChooserActionSelectFolder
				[("Select", ResponseAccept)]
    widgetShowAll fcd
    r <- dialogRun fcd
    when (r == ResponseAccept) $ do
         f <- fileChooserGetCurrentFolder fcd
         case f of
           Just dir -> do writeIORef dirRef dir
                          reloadDir
           Nothing -> return ()
    widgetDestroy fcd
 
  boxPackStart vbox dirChooser PackNatural 0
  boxPackStart vbox radioBox PackNatural 0
  boxPackStart vbox filesContainer PackGrow 0

  boxPackStart hbox vbox PackNatural 1
  boxPackStart hbox plotsContainer PackNatural 1
  boxPackStart hbox canvasContainer PackGrow 0
  
  window <- windowNew
  containerAdd window hbox
  onDestroy window mainQuit
  windowMaximize window
  widgetShowAll window
  mainGUI
