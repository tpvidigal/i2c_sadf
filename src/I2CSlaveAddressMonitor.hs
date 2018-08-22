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
idleFuncAddressMonitor :: Int a -> (a,a)
                                -> a 
                                -> ((a,a), a)
idleFuncAddressMonitor _ _ = ((0,0),0)

-- | Kernel function during start scenario
startFuncAddressMonitor :: Int a -> (a,a)
                                 -> a 
                                 -> ((a,a), a)
startFuncAddressMonitor _ sdaPosedge = ((0,sdaPosedge),0)

-- | Kernel function during get address scenario
getFuncAddressMonitor :: Int a -> (a,a)
                               -> a 
                               -> ((a,a), a)
getFuncAddressMonitor (counter, pastAddress) sdaPosedge = (feedback, readOp)
  where readOp   = sdaPosedge
        feedback = (counter+1,address)
        address  = pastAddress + sdaPosedge * (2^counter)

-- | Kernel function during match scenario
getFuncAddressMonitor :: Int a -> (a,a)
                               -> a 
                               -> ((a,a), a)
getFuncAddressMonitor _ sdaPosedge = (_, sdaPosedge)

-- | Create kernel
-- Arg 1: Control signal (scenario)
-- Arg 2: SDA value (posedge of SCL)
-- Ret 1: Feedback of address operation
-- Ret 2: Operation token
kernelAddressMonitor :: Int a => ScenarAddressMonitor
                              -> Signal a
                              -> (Signal (a,a), Signal a)
kernelAddressMonitor control sda = (feedback, readOp)
  where (feedback, readOp) = kernel32SADF control pastFeedback sda
        pastFeedback       = delaySADF initFeedback feedback
        initFeedback       = Signal (0,0)



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Idle scenario
idleScenar = ScenarAddressMonitor {
    inRates  = (1,0),
    outRates = (0,0),
    execFunc = idleFuncAddressMonitor
}

-- | Start scenario
startScenar = ScenarAddressMonitor {
    inRates  = (0,1),
    outRates = (1,0),
    execFunc = startFuncAddressMonitor
}

-- | Get address scenario
getScenar = ScenarAddressMonitor {
    inRates  = (0,1),
    outRates = (1,0),
    execFunc = getFuncAddressMonitor
}

-- | Match scenario
matchScenar = ScenarAddressMonitor {
    inRates  = (0,1),
    outRates = (0,1),
    execFunc = matchFuncAddressMonitor
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

