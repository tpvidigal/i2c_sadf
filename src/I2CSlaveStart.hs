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
  kernelStart,

  -- START condition detection function
  conditStart

) where

import I2CSlaveGlobals
import Data.Bits
import ForSyDe.Shallow
import SADF



---------------------------------------------------------
-- Kernel  
---------------------------------------------------------

-- | START condition definition
-- Basically a transition from (1,1) to (0,1)  will
-- define a START condition. So an inputSequence list
-- is created, which is a list of tuples that contains
-- the 'current' values of SDA/SCL and 'past' values.
-- That way, we can map a function that check those
-- values (checkStart) and generate a list of START
-- condition occurrences.
--
-- Arg1: Values of SDA and SCL
-- Ret1: START condition (or not)
conditStart :: [(Int,Int)] 
            -> [(Int,Int)] 
            -> [Int]
conditStart [] _   = []
conditStart inputs pastInputs = map checkStart inputSequence
  where inputSequence = (zip pastInputs inputs)
        checkStart (past, current)
          | past     /= (1,1) = 0
          | current  == (0,1) = 1
          | otherwise         = 0

-- | START scenario
startScenar :: ScenarCondition
startScenar = ((1,1), 1, conditStart)

-- | Create kernel
-- Arg 1:  SDA and SCL values
-- Return: If condition happened
--
-- NOTE: here we are creating an implicit FSM...
--       Is this allowed in the SADF framework
--       or should all the kernels behaviours
--       be stateless?
kernelStart :: Signal (Int,Int)
            -> Signal Int
kernelStart inputs = start
  where start      = kernel21SADF control inputs pastInputs
        control    = signal (repeat startScenar)
        pastInputs = delaySADF [(1,1)] inputs



