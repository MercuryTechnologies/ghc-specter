{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util.Plot
  ( -- * draw utilities
    makeSparkline,
  )
where

import Control.Monad.Extra (whenM)
import Data.Bits ((.|.))
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble (..), CFloat, CInt (..))
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (Ptr, nullPtr)
import ImGui
import ImPlot qualified
import ImPlot.Enum
import ImPlot.TH qualified as TH
import ImPlot.Template
import STD.Deletable (delete)
import Util.Orphans ()

-- this is a hack. but it's the best up to now.
TH.genPlotLineInstanceFor
  CPrim
  ( [t|Ptr CDouble|],
    TPInfo
      { tpinfoCxxType = "double",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "double"
      }
  )

-- this is a hack. but it's the best up to now.
TH.genPlotLineInstanceFor
  CPrim
  ( [t|Ptr CFloat|],
    TPInfo
      { tpinfoCxxType = "float",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "float"
      }
  )

-- this is a hack. but it's the best up to now.
TH.genPlotLine1InstanceFor
  CPrim
  ( [t|Ptr CFloat|],
    TPInfo
      { tpinfoCxxType = "float",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "float"
      }
  )

makeSparkline :: Ptr CFloat -> Int -> IO ()
makeSparkline pdat offset = do
  spark_size <- newImVec2 (-1) 35
  zero <- newImVec2 0 0
  ImPlot.pushStyleVar1 (fromIntegral (fromEnum ImPlotStyleVar_PlotPadding)) zero
  let spark_flags =
        fromIntegral $
          fromEnum ImPlotFlags_CanvasOnly
            .|. fromEnum ImPlotFlags_NoChild
      no_deco = fromIntegral (fromEnum ImPlotAxisFlags_NoDecorations)
  whenM (toBool <$> ImPlot.beginPlot ("1" :: CString) spark_size spark_flags) $ do
    ImPlot.setupAxes (nullPtr :: CString) (nullPtr :: CString) no_deco no_deco
    plotLine1 "##spark" pdat 100 1 0 0 (fromIntegral offset)
    ImPlot.endPlot
  delete spark_size
  delete zero
