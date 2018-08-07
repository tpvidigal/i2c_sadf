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
-- The I2C consists of a 2-wire bus. For simplicity, will consider only the
-- mandatory features listed on the specification document. A "slave" and a
-- "master" device are implemented. Also, only single master scenario will
-- be considered.
--
-----------------------------------------------------------------------------
--
-- Mandatory features
--
-- Single Master
-- * Send START condition (to start or restart communication)
-- * Send STOP condition (to stop a running communication)
-- * Receive acknowledge (may send if read operation, "0" for ACK)
-- * Send 7-bit slave address (only after a START)
--
-- Slave
-- * Detect START condition at any time (prepares to receive addr)
-- * Detect STOP condition at any time (stop completely)
-- * Send acknowledge (may receive if read operation, "0" for ACK)
-- * Contain 7-bit slave address (respond only to this address)
--
-----------------------------------------------------------------------------
--
-- Scenarios
-- 
-- Scenario 1: No communication stabilished
-- * Bus is stoped (SCL and SDA HIGH)
-- * Slaves can't do nothing
-- * If Master send START condition, go to scenario 1
--
-- Scenario 2: Reaching slave
-- * Master send slave address
-- * Slaves check if address match (send ACK) or not (send NACK)
-- * If only NACKs, go to scenario 1
-- * If ACK occur, go to scenario 3
--
-- Scenario 3: Communication stabilished
-- * Operations run as expected
-- * If START condition occur, go to scenario 2
-- * If STOP condition occur, go to scenario 1
-- * Else, stays on scenario 3
--
-----------------------------------------------------------------------------
--
-- SADF for I2C Slave
--
-- * There will be 2 kernels responsible for condition detection. They always
--   operate the same way (no detector necessary) and all other processes will
--   monitor those signals
--   > START condition monitor
--   > STOP condition monitor
--
-- * There will be 3 complex kernels. Each one requires a detector to model
--   the FSM they must obey. Also, they watch conditions to reset or start
--   their operation.
--   > Address + read/write selector monitor (OpAddress)
--   > Master read operator (OpRead)
--   > Master write operator (OpWrite)
--
-- * For simplicity, a stand-alone simple kernel will wait OpAddress output.
--   When it receives the token, it will ACK the master and send a token to
--   the corresponding operation (OpRead or OpWrite)
--   > ACK responder
--
-----------------------------------------------------------------------------

module I2CSlave (

  -- Example to show I2C operating
  runI2CSlaveExample

) where

import ForSyDe.Shallow
import SADF
import Data.Bits((.&.), (.|.), xor)



---------------------------------------------------------
-- Types and definitions
---------------------------------------------------------

-- | Slave address to be considered
SLAVEADDRESS :: Int
SLAVEADDRESS = 1;

-- | Scenario (rates) for a condition kernel
--
-- inRates = Consumption rates
--    1: SDA line value
--    2: SCL line value
-- outRates = Production rates
--    1: condition occurred
-- execFunc = Function that models operation
--    Arg 1:  previous lines' values
--    Arg 2:  lines' values token
--    Return: condition token
data ScenarCondition = ScenarCondition {
    inRates  :: (Int,Int), 
    outRates :: Int, 
    execFunc :: Int a => (a,a) 
                      -> (a,a) 
                      -> a 
} deriving (Show)

-- | Scenario (rates) for address operation kernel
--
-- inRates = Consumption rates
--    1: Feedback of process
--    2: Condition signal
--    3: SDA line value
-- outRates = Production rates
--    1: New feedback
--    2: read operation (or write)
-- execFunc = Function that models operation
--    Arg 1:  Counter and address feedback
--    Arg 2:  Start and stop conditions
--    Arg 3:  SDA values token
--    Retr 1: feedback with counter and address token
--    Retr 2: read operation (or write) token
data ScenarOpAddress = ScenarOpAddress {
    inRates  :: (Int,Int,Int),
    outRates :: (Int,Int),
    execFunc :: Int a => (a,a) 
                      -> (a,a)
                      -> a
                      -> ((a,a),a) 
} deriving (Show)



---------------------------------------------------------
-- START condition Kernel
---------------------------------------------------------

-- | START condition definition
-- Arg 1:  Past values of SDA and SCL
-- Arg 2:  New values of SDA and SCL
-- Return: START condition (or not)
conditStart :: Int a => (a,a) 
                     -> (a,a)
                     -> a
conditStart pastInputs newInputs = start
  where start
    | pastInputs != (1,1) = 0
    | newInputs  == (0,1) = 1
    | otherwise           = 0

-- | START condition scenario(s)
-- Always require new SDA/SCL values
-- Always generate the condition flag
-- Always act the same way
getScenarStart :: ScenarCondition
getScenarStart = ScenarCondition {
    inRates  = (1,1), 
    outRates = 1, 
    execFunc = conditStart 
}

-- | Create START kernel
-- Arg 1:  SDA and SCL values
-- Return: If condition happened
kernelStart :: Int a => Signal (a,a)
                     -> Signal a
kernelStart newInputs = start
  where start      = kernel21SADF scenar pastInputs newInputs
        pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)
        scenar     = Signal getScenarStart



---------------------------------------------------------
-- STOP condition Kernel
---------------------------------------------------------

-- | STOP condition definition
-- Arg 1:  Past values of SDA and SCL
-- Arg 2:  New values of SDA and SCL
-- Return: STOP condition (or not)
conditStop :: Int a => (a,a)
                    -> (a,a)
                    -> a
conditStop pastInputs newInputs = stop
  where stop
    | pastInputs != (0,1) = 0
    | newInputs  == (1,1) = 1
    | otherwise           = 0

-- | STOP condition scenario(s)
-- Always require new SDA/SCL values
-- Always generate the condition flag
-- Always act the same way
getScenarStop :: ScenarCondition
getScenarStop = ScenarCondition {
    inRates  = (1,1),
    outRates = 1,
    execFunc = conditStop
}

-- | Create STOP kernel
-- Arg 1:  SDA and SCL values
-- Return: If condition happened
kernelStop :: Int a => Signal (a,a)
                    -> Signal a
kernelStop newInputs = stop
  where stop       = kernel21SADF scenar pastInputs newInputs
        pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)
        scenar     = Signal getScenarStop



---------------------------------------------------------
-- Address Operation Detector
---------------------------------------------------------

-- | Address operation idle scenario
idleScenarOpAddress = ScenarOpAddress {
    inRates  = (0,1,0),
    outRates = (0,0),
    execFunc = opAddress
}

-- | Address operation running scenario
runScenarOpAddress = ScenarOpAddress {
    inRates  = (1,0,1),
    outRates = (1,0),
    execFunc = opAddress
}

-- | Address operation match scenario
matchScenarOpAddress = ScenarOpAddress {
    inRates  = (1,0,1),
    outRates = (1,1),
    execFunc = opAddress
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
  | start==1                               = runScenarOpAddress
  | otherwise                              = idleScenarOpAddress
nextScenarOpAddress runScenarOpAddress (counter, address) (start,stop)
  | stop ==1                               = idleScenarOpAddress
  | (counter==7) & (address==SLAVEADDRESS) = matchScenarOpAddress
  | otherwise                              = runScenarOpAddress;
nextScenarOpAddress matchScenarOpAddress _ (start,_)
  | start==1                               = runScenarOpAddress
  | otherwise                              = idleScenarOpAddress

-- | Detector for FSM of kernel
-- Inputs tokens
-- * Operation tuplet
--     > Address operation feedback
-- * Condition tuplet
--     > START condition
--     > STOP condition
-- * Wires tuplet
--     > SDA line
--     > SCL line
--
-- Outputs scenarios
-- * kernelOpAddress
detectOpAddress :: Int a => Signal (Signal a, Signal a)
                       -> Signal (Signal a, Signal a)
                       -> Signal (Signal (Signal a, Signal a), Signal a)
                       -> ScenarOpAddress
detectOpAddress = detector31SADF inRates stateTrans scenarSelect initState
  where inRates      = (1,0,0)
        stateTrans   = nextScenarOpAddress
        scenarSelect = stateTrans
        initState    = (inRates, initScenarOpAddress)

nextOpAddress = nextScenarOpAddress 



--------------------------------------------------------
-- Address Operation Kernel
---------------------------------------------------------

-- | Kernel operation definition
-- It mantains a fixed address that identifies the slave
-- When a new SDA value arrives (positive edge of SCL),
-- the feedback values are used
opAddress :: Int a -> (a,a)
                   -> (a,a)
                   -> a 
                   -> ((a,a), a)
opAddress (counter, pastAddress) (start,stop) sda = (feedback, readOp)
  where readOp = sda
        feedback
          | start  ==1 = (8,0)
          | stop   ==1 = (8,0)
          | counter==8 = (0,address)
          | otherwise  = (counter+1,address)
        address
          | counter==8 = sda
          | otherwise  = pastAddress + sda * (2^counter)

-- | Create kernel
-- Arg 1:  Scenario
-- Arg 2:  Condition signals
-- Arg 3:  SDA value (posedge of SCL)
-- Retr 1: Feedback of address operation
-- Retr 2: Operation token
kernelOpAddress :: Int a => ScenarAddress
                         -> Signal (Signal a, Signal a)
                         -> Signal a
                         -> ((Signal a, Signal a), Signal a)
kernelOpAddress scenar conditions sda = (feedback, readOp)
  where (feedback, readOp) = kernel32SADF scenar pastFeedback conditions sda
        pastFeedback       = delaySADF Signal initFeedback feedback
        initFeedback       = Signal ((8,0),0)



