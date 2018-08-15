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
-- Detector
---------------------------------------------------------

-- | Data at posedge idle scenario
idleScenarDataPosedge = ScenarCondition {
    inRates  = (1,1),
    outRates = 0,
    execFunc = dataPosedge
}

-- | Data at posedge got scenario
gotScenarDataPosedge = ScenarCondition {
    inRates  = (1,1),
    outRates = 1,
    execFunc = dataPosedge
}

-- | Next scenario(s)
-- Inputs
-- * Past values of SDA and SCL
-- * New values of SDA and SCL
-- Return
-- * SDA value at SCL posedge
nextScenarDataPosedge :: ScenarCondition
                      -> (Int, Int)
                      -> (int, int)
                      -> ScenarCondition
nextScenarDataPosedge _ pastInputs newInputs
    | pastInputs != (_,0) = idleScenarDataPosedge
    | newInputs  == (_,1) = gotScenarDataPosedge

-- | Detector for FSM of kernel
-- Inputs tokens
-- * Wires tuplet
--     > SDA line
--     > SCL line
detectDataPosedge :: Int a => Signal (a,a)
                           -> ScenarOpAddress
detectDataPosedge newInputs = detector21SADF inRates stateTrans scenarSelect initState pastInputs newInputs
  where inRates      = (1,1)
        stateTrans   = nextScenarDataPosedge
        scenarSelect = stateTrans
        initState    = (inRates, idleScenarDataPosedge)
        pastInputs   = delaySADF initInputs newInputs
        initInputs   = Signal (1,1)



---------------------------------------------------------
-- Kernel
---------------------------------------------------------

-- | Data at posedge of SCL definition
-- Arg 1:  Past values of SDA and SCL
-- Arg 2:  New values of SDA and SCL
-- Return: SDA value at SCL posedge
dataPosedge :: Int a => (a,a)
                     -> (a,a)
                     -> a
dataPosedge pastInputs newInputs = sdaPosedge
  where sdaPosedge
    | pastInputs != (_,0) = 0
    | newInputs  == (_,1) = snd newInputs
    | otherwise           = 0

-- | Create kernel
-- Arg 1:  Control signal (scenario)
-- Arg 2:  Line values (SDA and SCL)
-- Return: SDA value at SCL posedge
kernelDataPosedge :: Int a => ScenarCondition
                           -> Signal (a,a)
                           -> Signal a
kernelDataPosedge control newInputs = sdaPosedge
  where sdaPosedge = kernel21SADF scenar pastInputs newInputs
        pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)



