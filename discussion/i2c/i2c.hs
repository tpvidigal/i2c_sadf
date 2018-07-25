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
-- SADF chart for I2C Slave
--
-- Notes:
-- * [K] = kernel
-- * (D) = Detector
--
--                        ,----------------,
--                        |  ,-----,       |
--                        |  |     v       v
--       [K0]-----,       [K2]    [K3]    [K4]
--                |       ^  |    ^  |    ^  |
--                v       |  |    |  |    |  |
--                (D0)----'--|----'--|----'  |
--                ^  ^       |       |       |
--                |  |       |       |       |
--       [K1]-----'  '-------'-------'-------'
--
-- [K0]: START condition detector
-- [K1]: STOP condition detector
-- [K2]: Op address checker process
-- [K3]: Op read process
-- [K4]: Op write process
-- (D0): Controller
--
-- * If K0 detects a new START, D0 resets K3 and K4 and enables K2
-- * If K1 detects a STOP, D0 resets K2, K3 and K4
-- * When K2 identifies the correct address, it sends a token to K3 or K4
--   > depends of the operation defined by the master
-- * If K2, K3 or K4 sends a token to D0, it indicates slave must stop
--   > K2: if address doesn't match
--   > K3: if master sends NACK
--   > K4: if slave sends NACK
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
slaveAddress :: Int
slaveAddress = 1;

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
    inRates  :: (Int,Int),     -- ^ SDA and SCL rates
    outRates :: Int,           -- ^ Condition rate
    execFunc :: Int a => (a,a) -- ^ Past SDA/SCL
                      -> (a,a) -- ^ New SDA/SCL
                      -> a     -- ^ Condition
} deriving (Show)

-- | Scenario (rates) for a address operation kernel
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
    inRates  :: (Int,Int),     -- ^ SDA and SCL rates
    outRates :: Int,           -- ^ Condition rate
    execFunc :: Int a => (a,a) -- ^ Past SDA/SCL
                      -> (a,a) -- ^ New SDA/SCL
                      -> a     -- ^ Condition
} deriving (Show)



---------------------------------------------------------
-- START condition Kernel
---------------------------------------------------------

-- | START condition definition
conditStart :: Int a => (a,a) -- ^ Past values of SDA and SCL
                     -> (a,a) -- ^ New values of SDA and SCL
                     -> a     -- ^ START condition (or not)
conditStart pastInputs newInputs = start
  where start
    | pastInputs != (1,1) = 0
    | newInputs  == (0,1) = 1
    | otherwise           = 0

-- | START condition scenario(s)
scenarStart :: ScenarCondition
scenarStart = ScenarCondition {
    inRates  = (1,1),      -- ^ Always require new SDA/SCL values
    outRates = 1,          -- ^ Always generate the condition flag
    execFunc = conditStart -- ^ Always act the same way
}

-- | Create START kernel
kernelStart :: Int a => Signal (a,a) -- ^ SDA and SCL values
                     -> Signal a     -- ^ If condition happened
kernelStart newInputs = start
  where start      = kernel21SADF scenar pastInputs newInputs
        pastInputs = delaySADF initInputs newInputs
        initInputs = Signal (1,1)
        scenar     = Signal scenarStart



---------------------------------------------------------
-- STOP condition Kernel
---------------------------------------------------------

-- | STOP condition definition
conditStop :: Int a => (a,a) -- ^ Past values of SDA and SCL
                    -> (a,a) -- ^ New values of SDA and SCL
                    -> a     -- ^ STOP condition (or not)
conditStop pastInputs newInputs = stop
  where stop
    | pastInputs != (0,1) = 0
    | newInputs  == (1,1) = 1
    | otherwise           = 0

-- | STOP condition scenario(s)
scenarStop :: ScenarCondition
scenarStop = ScenarCondition {
    inRates  = (1,1),     -- ^ Always require new SDA/SCL values
    outRates = 1,         -- ^ Always generate the condition flag
    execFunc = conditStop -- ^ Always act the same way
}

-- | Create START kernel
kernelStop :: Int a => Signal (a,a) -- ^ SDA and SCL values
                    -> Signal a     -- ^ If condition happened
kernelStop newInputs = stop
  where stop       = kernel21SADF scenar pastInputs newInputs
        pastInputs = delaySADF Signal initInputs newInputs
        initInputs = Signal (1,1)
        scenar     = Signal scenarStop



---------------------------------------------------------
-- Address Kernel
---------------------------------------------------------

-- | Address Kernel definition
-- It mantains a fixed address that identifies the slave
-- When a new SDA value arrives (positive edge of SCL),
-- the feedback values are used
opAddress :: Int a => a             -- ^ Fixed adress of the slave
                   -> (a,a)         -- ^ Past counter and address values
                   -> a             -- ^ SDA line value (posedge of SCL)
                   -> ((a,a), a) -- ^ Feedback and output values
opAddress (counter, pastAddress) sda = (feedback, readOp)
  where readOp   = sda
        feedback = (counter+1, address)
        address  = pastAddress + sda * (2^counter)

-- | Address operation scenario(s)
scenarOpAddress :: (Int, Int)      -- ^ Bits counted and address received
                -> ScenarOpAddress -- ^ Next scenario
scenarOpAddress (8,address)
  | address == slaveAddress = ScenarOpAddress {
                                inRates  = 1,
                                outRates = 1,
                                execFunc = opAddress
                              }
  | otherwise               = ScenarOpAddress {
                                inRates  = 1,
                                outRates = 0,
                                execFunc = opAddress
                              }
scenarOpAddress (_,_) = ScenarOpAddress {
                          inRates  = 1,
                          outRates = 0,
                          execFunc = opAddress
                        }

-- | Create operation address kernel
kernelOpAddress :: ScenarAddress a -- ^ Scenario
                -> (Int,Int)       -- ^ SDA and SCL values
                -> Int             -- ^ Read operation (or write)
kernelOpAddress scenar newInputs = (match, readOp)
  where readOp = kernel21SADF scenar newInputs




