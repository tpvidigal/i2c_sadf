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
  kernelStop,

  -- STOP condition detection function
  conditStop

) where

import I2CSlaveGlobals
import Data.Bits
import ForSyDe.Shallow
import SADF



---------------------------------------------------------
-- Kernel  
---------------------------------------------------------

-- | STOP condition definition
-- Basically a transition from (0,1) to (1,1)  will
-- define a STOP condition. So an inputSequence list
-- is created, which is a list of tuples that contains
-- the 'current' values of SDA/SCL and 'past' values.
-- That way, we can map a function that check those
-- values (checkStop) and generate a list of STOP
-- condition occurrences.
--
-- Arg1: Values of SDA and SCL
-- Ret1: STOP condition (or not)
conditStop :: [(Int,Int)] 
           -> [Int]
conditStop []     = []
conditStop inputs = map checkStop inputSequence
  where pastInputs    = (1,1) : (init inputs)
        inputSequence = (zip pastInputs inputs)
        checkStop (past, current)
          | past     /= (0,1) = 0
          | current  == (1,1) = 1
          | otherwise         = 0

-- | STOP scenario
stopScenar :: ScenarCondition
stopScenar = (1, 1, conditStop)

-- | Scenarios for FSM of kernel
-- Create a list of the scenarios that the kernel will
-- be for each input combination. In this case, we
-- always are at the same scenario.
scenarios :: Signal (Int,Int)
          -> Signal ScenarCondition
scenarios inputs = signal $ take (lengthS inputs) $ repeat stopScenar

-- | Create kernel
-- Arg 1:  SDA and SCL values
-- Return: If condition happened
kernelStop :: Signal (Int,Int)
           -> Signal Int
kernelStop inputs = stop
  where stop      = kernel11SADF control inputs
        control   = scenarios inputs



