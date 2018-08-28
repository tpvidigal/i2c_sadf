-----------------------------------------------------------------------------
--
-- Module      :  I2C Master Write Operation block
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is the Master write operation I2C model following the SADF MoC.
-- If the address matches and the operation token indicates a write operation,
-- this block is activated. It receives 8 bits at the positive edge and then
-- responds if wants to receive more (ACK) or not (NACK).
--
-----------------------------------------------------------------------------

module I2CSlaveOpWrite (

  -- Master Write Operation kernel
  kernelOpWrite

) where

import I2CSlaveGlobals



--------------------------------------------------------
-- Kernel
--------------------------------------------------------

-- | Kernel function during idle scenario
idleFuncOpWrite :: Int a -> (a,a)
                         -> a 
                         -> a 
                         -> a 
                         -> ((a,a), a, a)
idleFuncOpWrite _ _ _ _ = ((0,0), 0, 0)

-- | Kernel function during start scenario
startFuncOpWrite :: Int a -> (a,a)
                          -> a 
                          -> a 
                          -> a 
                          -> ((a,a), a, a)
startFuncOpWrite _ _ _ _ = (feedback,0,sdaOut)
  where feedback = (0,0)
        sdaOut   = 0

-- | Kernel function during run scenario
runFuncOpWrite :: Int a -> (a,a)
                        -> a 
                        -> a 
                        -> a 
                        -> ((a,a), a, a)
runFuncOpWrite (counter,dataByte) sdaPosedge _ _ = (feedback,0,1)
  where feedback = (countback, databack)
        countback
          | counter == 0 = 7
          | otherwise    = counter-1
        databack = dataByte + dataBit
        dataBit
          | sdaPosedge == 1 = 2^countback
          | otherwise       = 0

-- | Kernel function during last scenario
lastFuncOpWrite :: Int a -> (a,a)
                         -> a 
                         -> a 
                         -> a 
                         -> ((a,a), a, a)
lastFuncOpWrite (_,dataByte) sdaPosedge _ _ = ((0,0),dataReceived,1)
  where dataReceived = dataByte + sdaPosedge

-- | Kernel function during ack scenario
ackFuncOpWrite :: Int a -> (a,a)
                        -> a 
                        -> a 
                        -> a 
                        -> ((a,a), a, a)
ackFuncOpWrite _ _ _ keep
  | keep == 1 = ((0,0),0,0)
  | otherwise = ((0,0),0,1)

-- | Create kernel
-- Arg1: Control signal (scenario)
-- Arg2: SDA value (posedge of SCL)
-- Arg3: SCL negedge
-- Arg4: Keep reading signal
-- Ret1: Feedback counter
-- Ret2: Received byte
-- Ret3: SDA of Slave
kernelOpWrite :: Int a => ScenarOpWrite
                       -> Signal a
                       -> Signal a
                       -> Signal a
                       -> (Signal a, Signal a, Signal a)
kernelOpWrite control sdaPosedge sclNegedge keep = (counter, dataReceived, sdaOut)
  where (feedback, dataReceived, sdaOut) = kernel43SADF control pastFeedback sdaPosedge sclNegedge keep
        feedback                         = (counter, dataByte)
        pastFeedback                     = delaySADF (0,0) feedback





