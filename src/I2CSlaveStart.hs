-----------------------------------------------------------------------------
--
-- Module      :  I2C START condition
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is the START condition kernel of I2C model following the SADF MoC.
-- It monitors the lines and sends a value '1' if the condition is detected.
--
-----------------------------------------------------------------------------

module I2CSlaveStart (

  -- Kernel of START condition
  kernelStart

) where

import ForSyDe.Shallow
import SADF
import I2CSlaveGlobals



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | START condition definition
-- Arg 1:  Past values of SDA and SCL
-- Arg 2:  New values of SDA and SCL
-- Return: START condition (or not)
conditStart :: Int a => (a,a) 
                     -> (a,a)
                     -> a
conditStart pastInputs newInputs = start
  where start
    | pastInputs != (1,1) = 0
    | newInputs  == (0,1) = 1
    | otherwise           = 0

-- | START condition scenario(s)
-- Always require new SDA/SCL values
-- Always generate the condition flag
-- Always act the same way
getScenarStart :: ScenarCondition
getScenarStart = ScenarCondition {
    inRates  = (1,1), 
    outRates = 1, 
    execFunc = conditStart 
}



---------------------------------------------------------
-- Kernel  
---------------------------------------------------------

-- | Create START kernel
-- Arg 1:  SDA and SCL values
-- Return: If condition happened
kernelStart :: Int a => Signal (a,a)
                     -> Signal a
kernelStart newInputs = start
  where start      = kernel21SADF scenar pastInputs newInputs
        pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)
        scenar     = Signal getScenarStart



