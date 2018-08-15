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
-- Detector
---------------------------------------------------------

-- | STOP condition definition
-- Arg 1:  Past values of SDA and SCL
-- Arg 2:  New values of SDA and SCL
-- Return: STOP condition (or not)
conditStop :: Int a => (a,a)
                    -> (a,a)
                    -> a
conditStop pastInputs newInputs = stop
  where stop
    | pastInputs != (0,1) = 0
    | newInputs  == (1,1) = 1
    | otherwise           = 0

-- | STOP condition scenario(s)
-- Always require new SDA/SCL values
-- Always generate the condition flag
-- Always act the same way
getScenarStop :: ScenarCondition
getScenarStop = ScenarCondition {
    inRates  = (1,1),
    outRates = 1,
    execFunc = conditStop
}



---------------------------------------------------------
-- Kernel  
---------------------------------------------------------

-- | Create STOP kernel
-- Arg 1:  SDA and SCL values
-- Return: If condition happened
kernelStop :: Int a => Signal (a,a)
                    -> Signal a
kernelStop newInputs = stop
  where stop       = kernel21SADF scenar pastInputs newInputs
        pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)
        scenar     = Signal getScenarStop




