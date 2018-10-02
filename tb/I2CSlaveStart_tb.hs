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

module I2CSlaveStart_tb
where

import I2CSlaveStart
import ForSyDe.Shallow

main = do let sda = [1,1,1,0,0,1,1,1,1,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,1,1]
          let scl = [1,1,1,1,0,1,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,1,0,1,1,1,1]
          let wireValues = zip sda scl
          let expected = conditStart wireValues
          let result   = fromSignal $ kernelStart $ signal wireValues
          putStrLn ""
          putStr "SDA:      "
          print sda
          putStr "SCL:      "
          print scl
          putStrLn ""
          putStr "Expected: "
          print expected
          putStr "Result:   "
          print result
          if(expected == result) then do
              putStrLn ""
              putStrLn "####### PASSED #######"
          else do
              putStr "Diff:     "
              putStrLn $ diffResult expected result
              putStrLn ""
              putStrLn "####### FAILED #######"
          putStrLn ""


diffResult :: [Int] -> [Int] -> String
diffResult exp res = markDiff (zip exp res)
  where markDiff [] = "_"
        markDiff ((x,y):xs)
          | x==y      = "__" ++ markDiff xs
          | otherwise = "_|" ++ markDiff xs
              

