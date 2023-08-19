{-# LANGUAGE OverloadedStrings #-}

module Demo
  ( demoLinePlots,
    demoTables,
  )
where

import Control.Monad.Extra (whenM)
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Data.IORef (IORef, readIORef)
import Data.String (IsString (..))
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble, CFloat)
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..))
import ImGui
import ImGui.Enum
import ImPlot qualified
import ImPlot.Enum
import ImPlot.Template
import Text.Printf (printf)
import Util (makeSparkline)

demoLinePlots :: (Ptr CFloat, Ptr CFloat) -> (Ptr CDouble, Ptr CDouble) -> IO ()
demoLinePlots (px1, py1) (px2, py2) = do
  _ <- begin ("Line plots" :: CString) nullPtr
  whenM (toBool <$> ImPlot.beginPlot_ ("Line Plots" :: CString)) $ do
    ImPlot.setupAxes
      ("x" :: CString)
      ("y" :: CString)
      (fromIntegral (fromEnum ImPlotAxisFlags_None))
      (fromIntegral (fromEnum ImPlotAxisFlags_None))
    t <- getTime
    for_ [0 .. 1000] $ \i -> do
      let x = fromIntegral i * 0.001
      pokeElemOff px1 i x
      pokeElemOff py1 i (0.5 + 0.5 * sin (50.0 * (x + realToFrac t / 10.0)))
    plotLine "f(x)" px1 py1 1001
    for_ [0 .. 19] $ \i -> do
      let x = fromIntegral i / 19.0
      pokeElemOff px2 i x
      pokeElemOff py2 i (x * x)
    plotLine "g(x)" px2 py2 20
    ImPlot.endPlot
  end

demoTables :: IORef Int -> Ptr CFloat -> IO ()
demoTables ref_offset pdat = do
  _ <- begin ("Table of plots" :: CString) nullPtr
  let flags =
        fromIntegral $
          fromEnum ImGuiTableFlags_BordersOuter
            .|. fromEnum ImGuiTableFlags_BordersV
            .|. fromEnum ImGuiTableFlags_RowBg
            .|. fromEnum ImGuiTableFlags_Resizable
            .|. fromEnum ImGuiTableFlags_Reorderable
  whenM (toBool <$> beginTable ("##table" :: CString) 3 flags) $ do
    tableSetupColumn ("Electrode" :: CString) (fromIntegral (fromEnum ImGuiTableColumnFlags_WidthFixed)) 75.0
    tableSetupColumn ("Voltage" :: CString) (fromIntegral (fromEnum ImGuiTableColumnFlags_WidthFixed)) 75.0
    tableSetupColumn_ ("EMG Signal" :: CString)
    tableHeadersRow
    offset <- readIORef ref_offset
    for_ [0 .. 9] $ \(row :: Int) -> do
      let offset' = (offset + row * 10) `mod` 100
      tableNextRow 0
      tableSetColumnIndex 0
      textUnformatted (fromString (printf "EMG %d" row) :: CString)
      tableSetColumnIndex 1
      val :: Float <- realToFrac <$> peekElemOff pdat offset'
      textUnformatted (fromString (printf "%.3f V" val) :: CString)
      tableSetColumnIndex 2
      pushID (fromIntegral row)
      makeSparkline pdat offset'
      popID
    endTable
  end
