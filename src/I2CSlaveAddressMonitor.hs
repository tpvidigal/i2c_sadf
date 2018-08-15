-----------------------------------------------------------------------------
--
-- Module      :  I2C Address Operation block
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is the address operation of I2C model following the SADF MoC.
-- When a START condition happens, it reads 7 posedge values respective to
-- the Slave Address that the Master wants to communicate. If matches, the
-- it sends a token.
--
-----------------------------------------------------------------------------

module I2CSlaveAddressMonitor (

  -- Address Operation kernel
  kernelAddressMonitor,

  -- Address Operation detector
  detectAddressMonitor

) where

import I2CSlaveGlobals



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Address operation idle scenario
idleScenar = ScenarAddressMonitor {
    inRates  = (0,1,0),
    outRates = (1,0),
    execFunc = addressMonitor
}

-- | Address operation running scenario
runScenar = ScenarAddressMonitor {
    inRates  = (0,0,1),
    outRates = (1,0),
    execFunc = addressMonitor
}

-- | Address operation match scenario
matchScenar = ScenarAddressMonitor {
    inRates  = (0,0,1),
    outRates = (1,1),
    execFunc = addressMonitor
}

-- | Next scenario(s)
-- Inputs
-- * Current scenario (state)
-- * Feedback: bits counted and address
-- * Condition: start and stop
-- Return
-- * Next scenario
nextScenar :: ScenarAddressMonitor
           -> (Int, Int)
           -> (int, int)
           -> ScenarAddressMonitor
nextScenar idleScenar _ (start,_)
  | start == 1                             = runScenar 
  | otherwise                              = idleScenar
nextScenar runScenar (counter, address) (start,stop)
  | stop == 1                              = idleScenar
  | (counter==6) & (address==SLAVEADDRESS) = matchScenar
  | otherwise                              = runScenar 
nextScenar matchScenar _ _                 = idleScenar

-- | Detector for FSM of kernel
-- Inputs tokens
-- * Operation tuplet
--     > Address operation feedback
-- * Condition tuplet
--     > START condition
--     > STOP condition
detectAddressMonitor :: Int a => Signal (a,a)
                              -> Signal (a,a)
                              -> ScenarAddressMonitor
detectAddressMonitor = detector21SADF inRates stateTrans scenarSelect initState currentState
  where inRates
          | scenarSelect==idleScenar = initRate
          | otherwise                = (1,0)
        stateTrans   = nextScenar
        scenarSelect = stateTrans
        initState    = (initRate, idleScenar)
        initRate     = (0,1)
        currentState = delay initState scenarSelect



--------------------------------------------------------
-- Kernel
---------------------------------------------------------

-- | Kernel operation definition
-- It mantains a fixed address that identifies the slave
-- When a new SDA value arrives (positive edge of SCL),
-- the feedback values are used.
addressMonitor :: Int a -> (a,a)
                        -> (a,a)
                        -> a 
                        -> ((a,a), a)
addressMonitor (counter, pastAddress) (start,stop) sdaPosedge = (feedback, readOp)
  where readOp = sdaPosedge
        feedback
          | start  ==1 = (7,0)
          | stop   ==1 = (7,0)
          | counter==7 = (0,address)
          | otherwise  = (counter+1,address)
        address
          | counter==7 = sdaPosedge
          | otherwise  = pastAddress + sdaPosedge * (2^counter)

-- | Create kernel
-- Arg 1:  Control signal (scenario)
-- Arg 2:  Condition signals
-- Arg 3:  SDA value (posedge of SCL)
-- Retr 1: Feedback of address operation
-- Retr 2: Operation token
kernelAddressMonitor :: Int a => ScenarAddressMonitor
                         -> Signal (a,a)
                         -> Signal a
                         -> (Signal (a,a), Signal a)
kernelAddressMonitor control conditions sda = (feedback, readOp, sdaOut)
  where (feedback, readOp) = kernel32SADF scenar pastFeedback conditions sda
        pastFeedback               = delaySADF initFeedback feedback
        initFeedback               = Signal (7,0)







