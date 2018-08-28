-----------------------------------------------------------------------------
--
-- Module      :  I2C Master SDA Manager block
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is the SDA Manager I2C model following the SADF MoC.
-- It monitor requests of change Slave's output SDA value from the operation
-- blocks. If a token with the value '0' from the active operation is received,
-- it forces the output to '0' as well. Otherwise, it's always '1'.
--
-----------------------------------------------------------------------------

module I2CSlaveSDAManager (

  -- SDA Manager kernel
  kernelSDAManager

) where

import I2CSlaveGlobals



--------------------------------------------------------
-- Kernel
--------------------------------------------------------

-- | Kernel function during idle scenario
idleFuncSDAManager :: Int a -> a
                            -> a 
                            -> a 
                            -> a 
idleFuncSDAManager _ _ _ = 1

-- | Kernel function during Master read scenario
readFuncSDAManager :: Int a -> a
                            -> a 
                            -> a 
                            -> a 
readFuncSDAManager _ sdaRead _ = sdaRead

-- | Kernel function during Master write scenario
writeFuncSDAManager :: Int a -> a
                             -> a 
                             -> a 
                             -> a 
writeFuncSDAManager _ _ sdaWrite = sdaWrite

-- | Create kernel
-- Arg1: Control signal (scenario)
-- Arg2: SCL negedge
-- Arg3: SDA request from read operation kernel
-- Arg4: SDA request from write operation kernel
-- Ret1: SDA output
kernelSDAManager :: Int a => ScenarSDAManager
                          -> Signal a
                          -> Signal a
                          -> Signal a
                          -> Signal a
kernelSDAManager = kernel31SADF





