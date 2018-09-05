-----------------------------------------------------------------------------
--
-- Module      :  I2C START condition testbench
-- Copyright   :  (c) Tiago Vidigal
-- License     :  still needs license
--
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- TB of START condition kernel of I2C model following the SADF MoC.
--
-----------------------------------------------------------------------------

module I2CSlaveStart_tb (

  -- Testbench run
  runI2CSlaveStart

) where

import I2CSlaveStart

runI2CSlaveStart :: Signal a => (a,a,a)
runI2CSlaveStart = (sda, scl, condition)
  where sda       = [1,1,1,0,0,1,1,1,1,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,1,1]
        scl       = [1,1,1,1,0,1,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,1,0,1,1,1,1]
        condition = kernelStart $ signal (getLines sda scl)

-----------------------------------------------------------------------------

getLines :: [a] -> [b] -> [(a,b)]
getLines (x:xs) (y:ys)
  | xs==ys==[] = [(x,y)]
  | xs==[]     = error "First array ended before second one" 
  | ys==[]     = error "Second array ended before first one" 
  | otherwise  = [(x,y)] ++ getLines xs ys






