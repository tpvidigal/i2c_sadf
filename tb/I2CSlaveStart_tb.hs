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
          putStrLn $ drawWave sda
          putStr "SCL:      "
          putStrLn $ drawWave scl
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
              

drawWave :: [Int] -> String
drawWave [] = ""
drawWave xs
  | head xs == 1 = "¨¨¨" ++ (stepWave xs)
  | head xs == 0 = "___" ++ (stepWave xs)
  | otherwise    = "XXX" ++ (stepWave xs)

stepWave :: [Int] -> String
stepWave []  = []
stepWave [_] = []
stepWave (x:y:xs) = step (x,y) ++ stepWave (y:xs)
  where step (0,0) = "__"
        step (0,1) = "/¨"
        step (1,0) = "\\_"
        step (1,1) = "¨¨"
        step _     = "XX"

