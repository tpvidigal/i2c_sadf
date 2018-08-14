-----------------------------------------------------------------------------
--
-- Module      :  I2C Communication Protocol
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- This is a I2C model following the SADF MoC. The objective is to produce a
-- model of a circuit to be used in the "test scenario generator" project.
--
-----------------------------------------------------------------------------

module I2CSlaveOpAddress (

  -- Address Operation kernel
  kernelOpAddress,

  -- Address Operation detector
  detectOpAddress

) where

import ForSyDe.Shallow
import SADF
import I2CSlaveGlobals
import Data.Bits((.&.), (.|.), xor)



---------------------------------------------------------
-- Detector
---------------------------------------------------------

-- | Address operation idle scenario
idleScenarOpAddress = ScenarOpAddress {
    inRates  = (0,1,0),
    outRates = (0,0,1),
    execFunc = opAddressMonitor
}

-- | Address operation running scenario
runScenarOpAddress = ScenarOpAddress {
    inRates  = (1,0,1),
    outRates = (1,0,1),
    execFunc = opAddressMonitor
}

-- | Address operation match scenario
matchScenarOpAddress = ScenarOpAddress {
    inRates  = (1,0,1),
    outRates = (0,0,1),
    execFunc = opAddressOperation
}

-- | Address operation ACK scenario
ackScenarOpAddress = ScenarOpAddress {
    inRates  = (0,1,0),
    outRates = (0,1,1),
    execFunc = opAddressOperation
}

-- | Next scenario(s)
-- Inputs
-- * Current scenario (state)
-- * Feedback: bits counted and address
-- * Condition: start and stop
-- Return
-- * Next scenario
nextScenarOpAddress :: ScenarOpAddress
                   -> (Int, Int)
                   -> (int, int)
                   -> ScenarOpAddress
nextScenarOpAddress idleScenarOpAddress _ (start,_)
  | start == 1                             = runScenarOpAddress
  | otherwise                              = idleScenarOpAddress
nextScenarOpAddress runScenarOpAddress (counter, address) (start,stop)
  | stop == 1                              = idleScenarOpAddress
  | (counter==7) & (address==SLAVEADDRESS) = matchScenarOpAddress
  | otherwise                              = runScenarOpAddress;
nextScenarOpAddress matchScenarOpAddress _ (start,stop)
  | start == 1                             = idleScenarOpAddress
  | stop == 1                              = idleScenarOpAddress
  | otherwise                              = ackScenarOpAddress
nextScenarOpAddress ackScenarOpAddress _ (start,stop)
  | start == 1                             = runScenarOpAddress
  | stop == 1                              = idleScenarOpAddress
  | otherwise                              = ackScenarOpAddress

-- | Detector for FSM of kernel
-- Inputs tokens
-- * Operation tuplet
--     > Address operation feedback
-- * Condition tuplet
--     > START condition
--     > STOP condition
detectOpAddress :: Int a => Signal ((a,a),a)
                         -> Signal (a,a)
                         -> ScenarOpAddress
detectOpAddress = detector31SADF inRates stateTrans scenarSelect initState currentState
  where inRates      = (1,0,0)
        stateTrans   = nextScenarOpAddress
        scenarSelect = stateTrans
        initState    = (inRates, idleScenarOpAddress)
        currentState = delay initState scenarSelect



--------------------------------------------------------
-- Kernel
---------------------------------------------------------

-- | Kernel operation definition
-- It mantains a fixed address that identifies the slave
-- When a new SDA value arrives (positive edge of SCL),
-- the feedback values are used. Also, the SDA generated
-- is defined for acknoledge (1 = NACK, 0 = ACK).
opAddress :: Int a -> (a,a)
                   -> (a,a)
                   -> a 
                   -> ((a,a), a, a)
opAddress (counter, pastAddress) (start,stop) sdaPosedge = (feedback, readOp, sdaOut)
  where readOp = sdaPosedge
        feedback
          | start  ==1 = (8,0)
          | stop   ==1 = (8,0)
          | counter==8 = (0,address)
          | otherwise  = (counter+1,address)
        address
          | counter==8 = sdaPosedge
          | otherwise  = pastAddress + sdaPosedge * (2^counter)
        sdaOut
          | counter==7 & pastAddress==SLAVEADDRESS = 0
          | otherwise                              = 1

-- | Create kernel
-- Arg 1:  Control signal (scenario)
-- Arg 2:  Condition signals
-- Arg 3:  SDA value (posedge of SCL)
-- Retr 1: Feedback of address operation
-- Retr 2: Operation token
-- Retr 3: SDA of Slave
kernelOpAddress :: Int a => ScenarOpAddress
                         -> Signal (a,a)
                         -> Signal a
                         -> (Signal (a,a), Signal a, Signal a)
kernelOpAddress control conditions sda = (feedback, readOp, sdaOut)
  where (feedback, readOp, sdaOut) = kernel33SADF scenar pastFeedback conditions sda
        pastFeedback               = delaySADF initFeedback feedback
        initFeedback               = Signal ((8,0),0,1)







