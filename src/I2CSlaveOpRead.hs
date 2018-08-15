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
-- this block is activated. It receives 8 bits at the positive edge and then
-- responds if wants to receive more (ACK) or not (NACK).
--
-----------------------------------------------------------------------------

module I2CSlaveOpRead (

  -- Master Read Operation kernel
  kernelOpRead,

  -- Master Read Operation detector
  detectOpRead

) where

import I2CSlaveGlobals



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Read operation idle scenario
idleScenar = ScenarOpRW {
    inRates  = (1,0,0,0,0),
    outRates = (0,1),
    execFunc = opRead
}

-- | Read operation running scenario
runScenar = ScenarOpRW {
    inRates  = (0,1,0,1,0),
    outRates = (1,1),
    execFunc = opRead
}

-- | Next scenario(s)
-- Inputs
-- * Current scenario (state)
-- * Feedback: bits counted and address
-- * Condition: start and stop
-- * Keep reading signal
-- Return
-- * Next scenario
nextScenar :: ScenarOpRW
           -> (Int, Int)
           -> (Int, Int)
           -> Int
           -> ScenarOpRW
nextScenar idleScenar (_,_) _ _ = runScenar
nextScenar runScenar  (counter,_) (start,stop) keepRead
  | start == 1                  = idleScenar
  | stop == 1                   = idleScenar
  | counter==7 & keepRead==0    = idleScenar
  | otherwise                   = runScenar

-- | Detector for FSM of kernel
-- Inputs tokens
-- * Operation tuplet
--     > Read operation feedback
-- * Condition tuplet
--     > START condition
--     > STOP condition
-- * Keep reading signal
detectOpRead :: Int a => Signal (a,a)
                      -> Signal (a,a)
                      -> Signal a
                      -> ScenarOpRW
detectOpRead = detector31SADF inRates stateTrans scenarSelect initState currentState
  where inRates      = (1,0,0)
        stateTrans   = nextScenar
        scenarSelect = stateTrans
        initState    = (inRates, idleScenar)
        currentState = delay initState scenarSelect



--------------------------------------------------------
-- Kernel
--------------------------------------------------------

-- | Kernel operation definition
-- When the operation value is received and it's read (1),
-- it read SDA value for 8 cycles and send an acknowledge
-- if should keep reading. Else send a NACK.
opRead :: Int a -> a
                -> (a,a)
                -> (a,a)
                -> a 
                -> a
                -> ((a,a), a)
opRead readOp (counter, pastData) (start,stop) sdaPosedge keepRead = (feedback, sdaOut)
  where feedback
          | start  ==1               = (8,0)
          | stop   ==1               = (8,0)
          | counter==8               = (0,databyte)
          | otherwise                = (counter+1,databyte)
        databyte                    
          | counter==8               = sdaPosedge
          | otherwise                = pastData + sdaPosedge * (2^counter)
        sdaOut
          | counter==7 & keepRead==1 = 0
          | otherwise                = 1

-- | Create kernel
-- Arg 1:  Control signal (scenario)
-- Arg 2:  Condition signals
-- Arg 3:  SDA value (posedge of SCL)
-- Retr 1: Feedback of address operation
-- Retr 2: Operation token
-- Retr 3: SDA of Slave
kernelOpRead :: Int a => ScenarOpRW
                         -> Signal (a,a)
                         -> Signal a
                         -> (Signal (a,a), Signal a, Signal a)
kernelOpRead control conditions sda = (feedback, readOp, sdaOut)
  where (feedback, readOp, sdaOut) = kernel33SADF scenar pastFeedback conditions sda
        pastFeedback               = delaySADF initFeedback feedback
        initFeedback               = Signal ((8,0),0,1)







