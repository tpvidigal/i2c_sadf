-----------------------------------------------------------------------------
--
-- Module      :  I2C Negative Edge
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is the negative edge monitor of I2C model following the SADF MoC.
-- When a negative edge is detected at the clock line (SCL), a token is sent.
--
-----------------------------------------------------------------------------

module I2CSlaveNegedge (

  -- Negative edge kernel
  kernelNegedge

) where

import I2CSlaveGlobals



---------------------------------------------------------
-- Kernel
---------------------------------------------------------

-- | Negedge of SCL definition
dataNegedge :: Int a => (a,a)
                     -> a
dataNegedge (sda,_) = sda

-- | Create kernel
-- Arg1: Line values (SDA and SCL)
-- Ret1: SCL negedge
kernelDataNegedge :: Int a => Signal (a,a)
                           -> Signal a
kernelDataNegedge lines = kernel11SADF control lines
  where control = detectDataNegedge lines



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Idle scenario
idleScenarNegedge = ScenarCondition {
    inRates  = (1,1),
    outRates = 0,
    execFunc = dataNegedge
}

-- | Got scenario
gotScenarNegedge = ScenarCondition {
    inRates  = (1,1),
    outRates = 1,
    execFunc = dataNegedge
}

-- | Next scenario(s)
-- Inputs
-- * Past values of SDA and SCL
-- * New values of SDA and SCL
nextScenarNegedge :: ScenarCondition
                  -> (Int, Int)
                  -> (int, int)
                  -> ScenarCondition
nextScenarNegedge _ pastInputs newInputs
    | pastInputs != (_,1) = idleScenarNegedge
    | newInputs  == (_,0) = gotScenarNegedge
    | otherwise           = idleScenarNegedge

-- | Detector's input rate
ratesNegedge = (0,1)

-- | Detector's scenario selection
selectNegedge :: ScenarCondition
              -> (Int, [ScenarCondition])
selectNegedge scenar = (1, [scenar])

-- | Detector for FSM of kernel
-- Inputs tokens
-- * Wires tuplet
--     > SDA line
--     > SCL line
detectDataNegedge :: Int a => Signal (a,a)
                           -> ScenarCondition
detectDataNegedge newInputs = detector21SADF ratesNegedge nextScenarNegedge selectNegedge idleScenarNegedge pastInputs newInputs
  where pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)



