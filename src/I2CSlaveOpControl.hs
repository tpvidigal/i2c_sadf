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
-- * SCL posedge
-- * Condition: start and stop
-- * Feedback counter
-- * Keep reading
-- * Matched address operation
-- Return
-- * Next scenario
nextScenarOpRead :: ScenarOpRead
                 -> Int
                 -> (Int, Int)
                 -> Int
                 -> Int
                 -> Int
                 -> ScenarOpRead
nextScenarOpRead idleScenarOpRead _ _ _ _ readOp
  | readOp == 1 = startScenarOpRead
  | otherwise   = idleScenarOpRead
nextScenarOpRead startScenarOpRead _ (start,stop) _ _ _
  | start == 1  = idleScenarOpRead
  | stop == 1   = idleScenarOpRead
  | otherwise   = runScenarOpRead
nextScenarOpRead runScenarOpRead _ (start,stop) count _ _
  | start == 1  = idleScenarOpRead
  | stop == 1   = idleScenarOpRead
  | count == 6  = lastScenarOpRead
  | otherwise   = runScenarOpRead
nextScenarOpRead lastScenarOpRead _ (start,stop) _ keep _
  | start == 1  = idleScenarOpRead
  | stop == 1   = idleScenarOpRead
  | keep == 0   = idleScenarOpRead
  | otherwise   = ackScenarOpRead
nextScenarOpRead ackScenarOpRead _ (start,stop) _ _ _
  | start == 1  = idleScenarOpRead
  | stop == 1   = idleScenarOpRead
  | otherwise   = runScenarOpRead

-- | Detector's input rate
ratesOpRead = (1,0,0,0,0)

-- | Detector's scenario selection
selectOpRead :: ScenarOpRead
             -> (Int, [ScenarOpRead])
selectOpRead scenar = (1, [scenar])



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
-- * SCL posedge
-- * Condition: start and stop
-- * Feedback: counter and data
-- * SDA (for ACK from Master)
-- * Matched address operation
-- Return
-- * Next scenario
nextScenarOpWrite :: ScenarOpWrite
                 -> Int
                 -> (Int, Int)
                 -> (Int, Int)
                 -> Int
                 -> Int
                 -> ScenarOpWrite
nextScenarOpWrite idleScenarOpWrite _ _ _ _ readOp
  | readOp == 0 = startScenarOpWrite
  | otherwise   = idleScenarOpWrite
nextScenarOpWrite startScenarOpWrite _ (start,stop) _ _ _
  | start == 1  = idleScenarOpWrite
  | stop == 1   = idleScenarOpWrite
  | otherwise   = runScenarOpWrite
nextScenarOpWrite runScenarOpWrite _ (start,stop) (count,_) _ _
  | start == 1  = idleScenarOpWrite
  | stop == 1   = idleScenarOpWrite
  | count == 6  = lastScenarOpWrite
  | otherwise   = runScenarOpWrite
nextScenarOpWrite lastScenarOpWrite _ (start,stop) _ _ _
  | start == 1  = idleScenarOpWrite
  | stop == 1   = idleScenarOpWrite
  | otherwise   = ackScenarOpWrite
nextScenarOpWrite ackScenarOpWrite _ (start,stop) _ sda _
  | start == 1  = idleScenarOpWrite
  | stop == 1   = idleScenarOpWrite
  | sda == 1    = idleScenarOpWrite
  | otherwise   = runScenarOpWrite

-- | Detector's input rate
ratesOpWrite = (1,0,0,0,0)

-- | Detector's scenario selection
selectOpWrite :: ScenarOpWrite
              -> (Int, [ScenarOpWrite])
selectOpWrite scenar = (1, [scenar])



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
-- * SCL posedge
-- * Condition: start and stop
-- * Matched address operation
-- Return
-- * Next scenario
nextScenarSDAManager :: ScenarSDAManager
                     -> Int
                     -> (Int, Int)
                     -> Int
                     -> ScenarSDAManager
nextScenarSDAManager idleScenarSDAManager _ _ readOp
  | readOp == 1 = readScenarSDAManager
  | otherwise   = writeScenarSDAManager
nextScenarSDAManager readScenarSDAManager _ (start,stop) _
  | start == 1  = idleScenarSDAManager
  | stop == 1   = idleScenarSDAManager
  | otherwise   = readScenarSDAManager
nextScenarSDAManager writeScenarSDAManager _ (start,stop) _
  | start == 1  = idleScenarSDAManager
  | stop == 1   = idleScenarSDAManager
  | otherwise   = writeScenarSDAManager

-- | Detector's input rate
ratesSDAManager = (1,0,0)

-- | Detector's scenario selection
selectSDAManager :: ScenarSDAManager
          -> (Int, [ScenarSDAManager])
selectSDAManager scenar = (1, [scenar])



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Operations Controller detector
-- Input tokens
-- * SDA line value (posedge of SCL)
-- * Condition: start and stop
-- * Feedback read
-- * Feedback write
-- * Keep reading
-- * Matched address operation
-- Output tokens (scenarios)
-- * Read kernel
-- * Write kernel
-- * SDA Manager kernel
detectOpControl :: Int a => Signal a
                         -> Signal (a,a)
                         -> Signal a
                         -> Signal (a,a)
                         -> Signal a
                         -> Signal a
                         -> (Signal ScenarOpRead,
                             Signal ScenarOpWrite,
                             Signal ScenarSDAManager)
detectOpControl allInputs = allOutputs
  where allInputs     = sdaPosedge conditions fbOpRead fbOpWrite keepRead readOp
        allOutputs    = (scenarRead, scenarWrite, scenarSDA)

        scenarRead    = detector51SADF ratesOpRead statesOpRead inputsOpRead
        statesOpRead  = nextScenarOpRead selectOpRead idleScenarOpRead
        inputsOpRead  = sdaPosedge conditions fbOpRead keepRead readOp

        scenarWrite   = detector51SADF ratesOpWrite statesOpWrite inputsOpWrite
        statesOpWrite = nextScenarOpWrite selectOpWrite idleScenarOpWrite
        inputsOpWrite = sdaPosedge conditions fbOpWrite keepRead readOp

        scenarSDA     = detector31SADF ratesSDAManager statesSDA inputsSDA
        statesSDA     = nextScenarSDAManager selectSDAManager idleScenarSDAManager
        inputsSDA     = sdaPosedge conditions readOp







