-----------------------------------------------------------------------------
--
-- Module      :  I2C STOP condition
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is the STOP condition kernel of I2C model following the SADF MoC.
-- It monitors the lines and sends a value '1' if the condition is detected.
--
-----------------------------------------------------------------------------

module I2CSlaveStop (

  -- Kernel of STOP condition
  kernelStop

) where

import I2CSlaveGlobals



---------------------------------------------------------
-- Kernel  
---------------------------------------------------------

-- | STOP condition definition
-- Arg1: Past values of SDA and SCL
-- Arg2: New values of SDA and SCL
-- Ret1: STOP condition (or not)
conditStop :: Int a => (a,a) 
                    -> (a,a)
                    -> a
conditStop pastInputs newInputs = stop
  where stop
    | pastInputs != (0,1) = 0
    | newInputs  == (1,1) = 1
    | otherwise           = 0

-- | Create kernel
-- Arg 1:  SDA and SCL values
-- Return: If condition happened
kernelStop :: Int a => Signal (a,a)
                    -> Signal a
kernelStop newInputs = stop
  where stop       = kernel21SADF control pastInputs newInputs
        pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)
        control    = detectStop



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Idle scenario
idleScenar = ScenarCondition {
    inRates  = (1,1),
    outRates = 0,
    execFunc = conditStop
}

-- | Got scenario
gotScenar = ScenarCondition {
    inRates  = (1,1),
    outRates = 1,
    execFunc = conditStop
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
    | pastInputs != (0,1) = idleScenar
    | newInputs  == (1,1) = gotScenar
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
detectStop :: Int a => Signal (a,a)
                    -> ScenarCondition
detectStop newInputs = detector21SADF rates nextScenar select idleScenar pastInputs newInputs
  where pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)



