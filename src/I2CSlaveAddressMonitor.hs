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



--------------------------------------------------------
-- Kernel
---------------------------------------------------------

-- | Kernel function during idle scenario
idleFunc :: Int a -> (a,a)
                  -> a 
                  -> ((a,a), a)
idleFunc _ _ = ((0,0),0)

-- | Kernel function during start scenario
startFunc :: Int a -> (a,a)
                   -> a 
                   -> ((a,a), a)
startFunc _ sdaPosedge = ((0,sdaPosedge),0)

-- | Kernel function during get address scenario
getFunc :: Int a -> (a,a)
                 -> a 
                 -> ((a,a), a)
getFunc (counter, pastAddress) sdaPosedge = (feedback, readOp)
  where readOp   = sdaPosedge
        feedback = (counter+1,address)
        address  = pastAddress + sdaPosedge * (2^counter)

-- | Kernel function during match scenario
matchFunc :: Int a -> (a,a)
                   -> a 
                   -> ((a,a), a)
matchFunc _ sdaPosedge = (_, sdaPosedge)

-- | Create kernel
-- Arg 1: Control signal (scenario)
-- Arg 2: SDA value (posedge of SCL)
-- Ret 1: Feedback of address operation
-- Ret 2: Operation token
kernelAddressMonitor :: Int a => ScenarAddressMonitor
                              -> Signal a
                              -> (Signal (a,a), Signal a)
kernelAddressMonitor control sda = (feedback, readOp)
  where (feedback, readOp) = kernel22SADF control pastFeedback sda
        pastFeedback       = delaySADF initFeedback feedback
        initFeedback       = Signal (0,0)



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Idle scenario
idleScenar = ScenarAddressMonitor {
    inRates  = (1,0),
    outRates = (0,0),
    execFunc = idleFunc
}

-- | Start scenario
startScenar = ScenarAddressMonitor {
    inRates  = (0,1),
    outRates = (1,0),
    execFunc = startFunc
}

-- | Get address scenario
getScenar = ScenarAddressMonitor {
    inRates  = (0,1),
    outRates = (1,0),
    execFunc = getFunc
}

-- | Match scenario
matchScenar = ScenarAddressMonitor {
    inRates  = (0,1),
    outRates = (0,1),
    execFunc = matchFunc
}

-- | Next scenario(s)
-- Inputs
-- * Current scenario (state)
-- * SCL posedge
-- * Feedback: bits counted and address
-- * Condition: start and stop
-- Return
-- * Next scenario
nextScenar :: ScenarAddressMonitor
           -> Int
           -> (Int, Int)
           -> (int, int)
           -> ScenarAddressMonitor
nextScenar idleScenar _ _ (start,_)
  | start == 1                        = startScenar 
  | otherwise                         = idleScenar
nextScenar startScenar _ _ (start,stop)
  | start == 1                        = idleScenar
  | stop == 1                         = idleScenar
  | otherwise                         = getScenar
nextScenar getScenar _ (count, addr) (start,stop)
  | start == 1                        = idleScenar
  | stop == 1                         = idleScenar
  | (count==6) & (addr==SLAVEADDRESS) = matchScenar
  | otherwise                         = idleScenar 
nextScenar matchScenar _ _ _          = idleScenar

-- | Detector's input rate
rates = (1,0,0)

-- | Detector's scenario selection
select :: ScenarAddressMonitor
       -> (Int, [ScenarAddressMonitor])
select scenar = (1, [scenar])

-- | Detector for FSM of kernel
-- Inputs tokens
-- * SCL posedge
-- * Feedback: bits counted and address
-- * Condition: start and stop
-- Output tokens (scenarios)
-- * Address Monitor kernel
detectAddressMonitor :: Int a => Signal a
                              -> Signal (a,a)
                              -> Signal (a,a)
                              -> ScenarAddressMonitor
detectAddressMonitor = detector31SADF rates nextScenar select idleScenar

