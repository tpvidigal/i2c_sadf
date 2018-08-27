-----------------------------------------------------------------------------
--
-- Module      :  I2C global definitions
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- These are all global definitions used in the I2C following the SADF MoC. 
--
-----------------------------------------------------------------------------

module I2CSlaveGlobals (

  -- Address of the slave
  SLAVEADDRESS,

  -- Scenario of condition kernels
  ScenarioCondition,

  -- Scenario of Address Monitor kernel
  ScenarioAddressMonitor,

  -- Scenario of Read operation kernel
  ScenarioOpRead,

  -- Scenario of Write operation kernel
  ScenarioOpWrite

) where

import ForSyDe.Shallow
import SADF
import Data.Bits



---------------------------------------------------------
-- Constants
---------------------------------------------------------

-- | Slave address to be considered
SLAVEADDRESS :: Int
SLAVEADDRESS = 1;



---------------------------------------------------------
-- Scenario types
---------------------------------------------------------

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

-- | Scenario (rates) for address monitor kernel
--
-- inRates = Consumption rates
--    1: Feedback of process
--    2: SDA line value (posedge of SCL)
-- outRates = Production rates
--    1: New feedback
--    2: Read operation (or write)
-- execFunc = Function that models operation
data ScenarAddressMonitor = ScenarAddressMonitor {
    inRates  :: (Int,Int),
    outRates :: (Int,Int),
    execFunc :: Int a => (a,a) 
                      -> a
                      -> ((a,a),a) 
} deriving (Show)

-- | Scenario (rates) for SDA Manager kernel
--
-- inRates = Consumption rates
--    1: SDA from read operation kernel
--    2: SDA from write operation kernel
-- outRates = Production rates
--    1: SDA output of slave
-- execFunc = Function that models operation
data ScenarSDAManager = ScenarSDAManager {
    inRates  :: (Int,Int),
    outRates :: Int,
    execFunc :: Int a => a 
                      -> a
                      -> a
} deriving (Show)

-- | Scenario (rates) for read operation kernels
--
-- inRates = Consumption rates
--    1: Feedback counter
--    2: SDA line value (posedge of SCL)
--    3: SCL negative edge
--    4: Byte to send
-- outRates = Production rates
--    1: New feedback counter
--    2: Done signal
--    3: SDA value from Slave
-- execFunc = Function that models operation
data ScenarOpRead = ScenarOpRead {
    inRates  :: (Int,Int,Int,Int),
    outRates :: (Int,Int,Int),
    execFunc :: Int a => a
                      -> a
                      -> a
                      -> a
                      -> (a,a,a) 
} deriving (Show)

-- | Scenario (rates) for write operation kernels
--
-- inRates = Consumption rates
--    1: Feedback: counter and data
--    2: SDA line value (posedge of SCL)
--    3: SCL negative edge
--    3: Keep reading signal
-- outRates = Production rates
--    1: New feedback
--    2: Received byte
--    3: SDA value from Slave
-- execFunc = Function that models operation
data ScenarOpWrite = ScenarOpWrite {
    inRates  :: (Int,Int,Int,Int),
    outRates :: (Int,Int,Int),
    execFunc :: Int a => (a,a)
                      -> a
                      -> a
                      -> a
                      -> ((a,a),a,a) 
} deriving (Show)










