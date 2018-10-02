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
  slaveAddress,

  -- Scenario of condition kernels
  ScenarCondition (..),

  -- Scenario of Address Monitor kernel
  ScenarAddressMonitor (..),

  -- Scenario of Read operation kernel
  ScenarOpRead (..),

  -- Scenario of Write operation kernel
  ScenarOpWrite (..)

) where

import Text.Show.Functions


---------------------------------------------------------
-- Constants
---------------------------------------------------------

-- | Slave address to be considered
slaveAddress :: Num a => a
slaveAddress = 1



---------------------------------------------------------
-- Scenario types
---------------------------------------------------------

-- | Scenario (rates) for a condition kernel
--
-- inRates = Consumption rates
--    1: SDA and SCL lines value
-- outRates = Production rates
--    1: condition occurred
-- execFunc = Function that models operation
--    Arg 1:  lines' values token
--    Return: condition token
type ScenarCondition = (Int, Int, [(Int,Int)] -> [Int])

-- | Scenario (rates) for address monitor kernel
--
-- inRates = Consumption rates
--    1: Feedback of process
--    2: SDA line value (posedge of SCL)
-- outRates = Production rates
--    1: New feedback
--    2: Read operation (or write)
-- execFunc = Function that models operation
type ScenarAddressMonitor = ((Int,Int), (Int,Int), [(Int,Int)] -> [Int] -> [((Int,Int),Int)])

-- | Scenario (rates) for read operation kernel
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
type ScenarOpRead = ((Int,Int,Int,Int), (Int,Int,Int), [Int] -> [Int] -> [Int] -> [Int] -> [(Int,Int,Int)])

-- | Scenario (rates) for write operation kernel
--
-- inRates = Consumption rates
--    1: Feedback: counter and data
--    2: SDA line value (posedge of SCL)
--    3: SCL negative edge
--    4: Keep reading signal
-- outRates = Production rates
--    1: New feedback
--    2: Received byte
--    3: SDA value from Slave
-- execFunc = Function that models operation
type ScenarOpWrite = ((Int,Int,Int,Int), (Int,Int,Int), [(Int,Int)] -> [Int] -> [Int] -> [Int] -> [((Int,Int),Int,Int)])

-- | Scenario (rates) for SDA Manager kernel
--
-- inRates = Consumption rates
--    1: SCL negedge
--    2: SDA from Master Read operation kernel
--    3: SDA from Master Write operation kernel
-- outRates = Production rates
--    1: SDA output of Slave
-- execFunc = Function that models operation
type ScenarSDAManager = ((Int,Int,Int), Int, [Int] -> [Int] -> [Int] -> [Int])








