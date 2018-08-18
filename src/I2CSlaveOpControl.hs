-----------------------------------------------------------------------------
--
-- Module      :  I2C Operation Controller block
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is the operation controller of I2C model following the SADF MoC.
-- If address monitor sends a token, this controller is activated.
-- It identifies the operation (read or write) and updates the scenario of
-- the operation kernels. Also, it enables the SDA Manager to drive the
-- slave's SDA line.
--
-----------------------------------------------------------------------------

module I2CSlaveOpControl (

  -- Operation Controller detector
  detectOpControl

) where

import I2CSlaveGlobals



---------------------------------------------------------
-- Scenarios: Read Operation
---------------------------------------------------------

-- | Idle scenario
idleScenarOpRead = ScenarOpRead {
    inRates  = (0,0,0),
    outRates = (0,0,0),
    execFunc = idleFuncOpRead
}

-- | Start scenario
startScenarOpRead = ScenarOpRead {
    inRates  = (0,1,0),
    outRates = (1,0,1),
    execFunc = startFuncOpRead
}

-- | Receiving byte scenario
runScenarOpRead = ScenarOpRead {
    inRates  = (0,1,0),
    outRates = (1,0,0),
    execFunc = runFuncOpRead
}

-- | Last bit scenario
lastScenarOpRead = ScenarOpRead {
    inRates  = (0,1,0),
    outRates = (1,1,1),
    execFunc = ackFuncOpRead
}

-- | ACK scenario
ackScenarOpRead = ScenarOpRead {
    inRates  = (0,1,0),
    outRates = (1,0,0),
    execFunc = ackFuncOpRead
}

-- | Next scenario(s)
-- Inputs
-- * Current scenario (state)
-- * Condition: start and stop
-- * Feedback counter
-- * Keep reading
-- * Matched address operation
-- Return
-- * Next scenario
nextScenarOpRead :: ScenarOpRead
                 -> (Int, Int)
                 -> Int
                 -> Int
                 -> Int
                 -> ScenarOpRead
nextScenarOpRead idleScenarOpRead _ _ _ readOp
  | readOp == 1 = startScenarOpRead
  | otherwise   = idleScenarOpRead
nextScenarOpRead startScenarOpRead (start,stop) _ _ _
  | start == 1  = idleScenarOpRead
  | stop == 1   = idleScenarOpRead
  | otherwise   = runScenarOpRead
nextScenarOpRead runScenarOpRead (start,stop) count _ _
  | start == 1  = idleScenarOpRead
  | stop == 1   = idleScenarOpRead
  | count == 6  = lastScenarOpRead
  | otherwise   = runScenarOpRead
nextScenarOpRead lastScenarOpRead (start,stop) _ keep _
  | start == 1  = idleScenarOpRead
  | stop == 1   = idleScenarOpRead
  | keep == 0   = idleScenarOpRead
  | otherwise   = ackScenarOpRead
nextScenarOpRead ackScenarOpRead (start,stop) _ _ _
  | start == 1  = idleScenarOpRead
  | stop == 1   = idleScenarOpRead
  | otherwise   = runScenarOpRead

-- | Detector's input rate
-- Idle:  Wait matched address operation
-- Other: Watch feedback's counter
inRatesOpRead :: Int a => ScenarOpRead
                       -> (a,a,a,a)
inRatesOpRead idleScenarOpRead = (0,0,0,1)
inRatesOpRead _                = (1,0,0,0)



---------------------------------------------------------
-- Scenarios: Write Operation
---------------------------------------------------------

-- | Idle scenario
idleScenarOpWrite = ScenarOpWrite {
    inRates  = (0,0,0),
    outRates = (0,0,0),
    execFunc = idleFuncOpWrite
}

-- | Start scenario
startScenarOpWrite = ScenarOpWrite {
    inRates  = (0,1,0),
    outRates = (1,0,1),
    execFunc = startFuncOpWrite
}

-- | Receiving byte scenario
runScenarOpWrite = ScenarOpWrite {
    inRates  = (0,1,0),
    outRates = (1,0,1),
    execFunc = runFuncOpWrite
}

-- | Last bit scenario
lastScenarOpWrite = ScenarOpWrite {
    inRates  = (0,1,0),
    outRates = (1,1,1),
    execFunc = lastFuncOpWrite
}

-- | ACK scenario
ackScenarOpWrite = ScenarOpWrite {
    inRates  = (0,1,0),
    outRates = (1,1,0),
    execFunc = ackFuncOpWrite
}

-- | Next scenario(s)
-- Inputs
-- * Current scenario (state)
-- * Condition: start and stop
-- * Feedback: counter and data
-- * SDA (for ACK from Master)
-- * Matched address operation
-- Return
-- * Next scenario
nextScenarOpWrite :: ScenarOpWrite
                 -> (Int, Int)
                 -> (Int, Int)
                 -> Int
                 -> Int
                 -> ScenarOpWrite
nextScenarOpWrite idleScenarOpWrite _ _ _ readOp
  | readOp == 0 = startScenarOpWrite
  | otherwise   = idleScenarOpWrite
nextScenarOpWrite startScenarOpWrite (start,stop) _ _ _
  | start == 1  = idleScenarOpWrite
  | stop == 1   = idleScenarOpWrite
  | otherwise   = runScenarOpWrite
nextScenarOpWrite runScenarOpWrite (start,stop) (count,_) _ _
  | start == 1  = idleScenarOpWrite
  | stop == 1   = idleScenarOpWrite
  | count == 6  = lastScenarOpWrite
  | otherwise   = runScenarOpWrite
nextScenarOpWrite lastScenarOpWrite (start,stop) _ _ _
  | start == 1  = idleScenarOpWrite
  | stop == 1   = idleScenarOpWrite
  | otherwise   = ackScenarOpWrite
nextScenarOpWrite ackScenarOpWrite (start,stop) _ sda _
  | start == 1  = idleScenarOpWrite
  | stop == 1   = idleScenarOpWrite
  | sda == 1    = idleScenarOpWrite
  | otherwise   = runScenarOpWrite

-- | Detector's input rate
-- Idle:  Wait matched address operation
-- Other: Watch feedback's counter
inRatesOpWrite :: Int a => ScenarOpWrite
                        -> (a,a,a,a)
inRatesOpWrite idleScenarOpWrite = (0,0,0,1)
inRatesOpWrite _                 = (1,0,0,0)



---------------------------------------------------------
-- Scenarios: SDA Manager
---------------------------------------------------------

-- | Idle scenario
idleScenarSDAManager = ScenarSDAManager {
    inRates  = (0,0),
    outRates = 0,
    execFunc = idleFuncSDAManager
}

-- | Read scenario
readScenarSDAManager = ScenarSDAManager {
    inRates  = (1,0),
    outRates = 1,
    execFunc = readFuncSDAManager
}

-- | Write scenario
writeScenarSDAManager = ScenarSDAManager {
    inRates  = (0,1),
    outRates = 1,
    execFunc = writeFuncSDAManager
}

-- | Next scenario(s)
-- Inputs
-- * Current scenario (state)
-- * Condition: start and stop
-- * Matched address operation
-- Return
-- * Next scenario
nextScenarSDAManager :: ScenarSDAManager
                 -> (Int, Int)
                 -> Int
                 -> ScenarSDAManager
nextScenarSDAManager idleScenarSDAManager _ readOp
  | readOp == 1 = readScenarSDAManager
  | otherwise   = writeScenarSDAManager
nextScenarSDAManager readScenarSDAManager (start,stop) _
  | start == 1  = idleScenarSDAManager
  | stop == 1   = idleScenarSDAManager
  | otherwise   = readScenarSDAManager
nextScenarSDAManager writeScenarSDAManager (start,stop) _
  | start == 1  = idleScenarSDAManager
  | stop == 1   = idleScenarSDAManager
  | otherwise   = writeScenarSDAManager

-- | Detector's input rate
-- Idle:  Wait matched address operation
-- Other: Watch conditions
inRatesSDAManager :: Int a => ScenarSDAManager
                           -> (a,a)
inRatesSDAManager idleScenarSDAManager = (0,1)
inRatesSDAManager _                    = (1,0)



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Operations Controller detector
-- Input tokens
-- * Condition: start and stop
-- * SDA line value (posedge of SCL)
-- * Keep reading
-- * Matched address operation
-- Output tokens (scenarios)
-- * Read kernel
-- * Write kernel
-- * SDA Manager kernel
detectOpControl :: Int a => Signal(a,a)
                         -> Signal a
                         -> Signal a
                         -> Signal a
                         -> (ScenarOpRead,
                             ScenarOpWrite,
                             ScenarSDAManager)
detectOpControl (start,stop) sdaPosedge keepRead readOp = (scenarRead, scenarWrite, scenarSDA)
  where scenarRead = detector??SADF




