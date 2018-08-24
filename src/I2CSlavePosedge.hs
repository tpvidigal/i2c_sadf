-----------------------------------------------------------------------------
--
-- Module      :  I2C Positive Edge
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is the positive edge monitor of I2C model following the SADF MoC.
-- When a positive edge is detected at the clock line (SCL), the value at the
-- data line (SDA) is sent.
--
-----------------------------------------------------------------------------

module I2CSlavePosedge (

  -- Positive edge kernel
  kernelPosedge

) where

import I2CSlaveGlobals



---------------------------------------------------------
-- Kernel
---------------------------------------------------------

-- | Data at posedge of SCL definition
-- Arg1: Line values of SDA and SCL
-- Ret1: SDA value at SCL posedge
dataPosedge :: Int a => (a,a)
                     -> a
dataPosedge (sda,_) = sda

-- | Create kernel
-- Arg1: Line values (SDA and SCL)
-- Ret1: SDA value at SCL posedge
kernelDataPosedge :: Int a => Signal (a,a)
                           -> Signal a
kernelDataPosedge lines = kernel11SADF control lines
  where control = detectDataPosedge lines



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Idle scenario
idleScenar = ScenarCondition {
    inRates  = (1,1),
    outRates = 0,
    execFunc = dataPosedge
}

-- | Got scenario
gotScenar = ScenarCondition {
    inRates  = (1,1),
    outRates = 1,
    execFunc = dataPosedge
}

-- | Next scenario(s)
-- Inputs
-- * Past values of SDA and SCL
-- * New values of SDA and SCL
nextScenar :: ScenarCondition
           -> (Int, Int)
           -> (int, int)
           -> ScenarCondition
nextScenar _ pastInputs newInputs
    | pastInputs != (_,0) = idleScenar
    | newInputs  == (_,1) = gotScenar
    | otherwise           = idleScenar

-- | Detector's input rate
rates = (0,1)

-- | Detector's scenario selection
select :: ScenarCondition
       -> (Int, [ScenarCondition])
select scenar = (1, [scenar])

-- | Detector for FSM of kernel
-- Inputs tokens
-- * Wires tuplet
--     > SDA line
--     > SCL line
detectDataPosedge :: Int a => Signal (a,a)
                           -> ScenarCondition
detectDataPosedge newInputs = detector21SADF rates nextScenar select idleScenar pastInputs newInputs
  where pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)



