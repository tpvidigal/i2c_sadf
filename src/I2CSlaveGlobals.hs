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

  -- Scenario of Address Operation kernel
  ScenarioOpAddress

) where



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

-- | Scenario (rates) for address operation kernel
--
-- inRates = Consumption rates
--    1: Feedback of process
--    2: Condition signal
--    3: SDA line value (posedge of SCL)
-- outRates = Production rates
--    1: New feedback
--    2: read operation (or write)
--    3: SDA value from Slave
-- execFunc = Function that models operation
data ScenarOpAddress = ScenarOpAddress {
    inRates  :: (Int,Int,Int),
    outRates :: (Int,Int,Int),
    execFunc :: Int a => (a,a) 
                      -> (a,a)
                      -> a
                      -> ((a,a),a,a) 
} deriving (Show)



