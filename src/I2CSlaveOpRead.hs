-----------------------------------------------------------------------------
--
-- Module      :  I2C Master Read Operation block
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is the Master read operation I2C model following the SADF MoC.
-- If the address matches and the operation token indicates a read operation,
-- this block is activated. It transmits 8 bits at the positive edge and then
-- receives if Master wants to more (ACK) or not (NACK).
--
-----------------------------------------------------------------------------

module I2CSlaveOpRead (

  -- Master Read Operation kernel
  kernelOpRead

) where

import I2CSlaveGlobals



--------------------------------------------------------
-- Kernel
--------------------------------------------------------

-- | Kernel function during idle scenario
idleFunc :: Int a -> a
                  -> a 
                  -> a 
                  -> a 
                  -> (a, a, a)
idleFunc _ _ _ _ = (0, 0, 0)

-- | Kernel function during start scenario
startFunc :: Int a -> a
                   -> a 
                   -> a 
                   -> a 
                   -> (a, a, a)
startFunc _ _ _ _ = (feedback,0,sdaOut)
  where feedback = 0
        sdaOut   = 0

-- | Kernel function during run scenario
runFunc :: Int a -> a
                 -> a 
                 -> a 
                 -> a 
                 -> (a, a, a)
runFunc counter _ _ dataByte = (feedback,0,sdaOut)
  where feedback
          | counter == 0 = 7
          | otherwise    = counter-1
        sdaOut 
          | testBit feedback dataByte = 1
          | otherwise                 = 0

-- | Kernel function during last scenario
lastFunc :: Int a -> a
                  -> a 
                  -> a 
                  -> a 
                  -> (a, a, a)
lastFunc _ _ _ dataByte = (0,1,sdaOut)
  where sdaOut 
          | testBit 0 dataByte = 1
          | otherwise          = 0

-- | Kernel function during ack scenario
ackFunc :: Int a -> a
                 -> a 
                 -> a 
                 -> a 
                 -> (a, a, a)
ackFunc _ _ _ _ = (0,1,1)

-- | Create kernel
-- Arg1: Control signal (scenario)
-- Arg2: SDA value (posedge of SCL)
-- Arg3: SCL negedge
-- Arg4: Data byte to send
-- Ret1: Feedback counter
-- Ret2: Done signal
-- Ret3: SDA of Slave
kernelOpRead :: Int a => ScenarOpRead
                      -> Signal a
                      -> Signal a
                      -> Signal a
                      -> (Signal a, Signal a, Signal a)
kernelOpRead control sdaPosedge sclNegedge dataByte = (feedback, done, sdaOut)
  where (feedback, readOp, sdaOut) = kernel43SADF control pastFeedback sdaPosedge sclNegedge dataByte
        pastFeedback               = delaySADF 0 feedback







