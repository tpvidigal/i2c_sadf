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

--------------------------------------------
-- Scenario (rates) for a condition kernel
--
-- inRates = Consumption rates
--    1: lines' (SDA and SCL) values
-- outRates = Production rates
--    1: condition occurred
-- execFunc = Function that models operation
--    Arg 1:  previous lines' values
--    Arg 2:  lines' values token
--    Return: condition token
data scenarCondition = scenarCondition {
    inRates  :: Int,
    outRates :: Int,
    execFunc :: Int a => (a,a) -> (a,a) -> a
} deriving (Show)



---------------------------------------------------------
-- START condition Kernel
---------------------------------------------------------

conditionStart :: Int a => (a,a) -> (a,a) -> a
conditionStart

startKernel :: Numb => Signal 






---------------------------------------------------------
-- Instruction Fetch (IF) Kernel
---------------------------------------------------------

-- | 'ifScenario' is the list of possible scenarios for the IF kernel
ifScenario :: Int -> ((Int, Int, Int), (Int, Int, Int, Int), [Int]
           -> [Int] -> [Vector String] -> ([[Int]], [String], [Int], [Vector String]))
ifScenario n
  | n > 20 || n < 0   = error "ifScenario: Non existent scenario"
  | n >= 14 && n < 19 = ((1,1,1), (1,1,1,1), \[a] [pc] [m] -> ([arg m (pc+a)], [op m (pc+a)], [pc+a+1], [m])) -- branch
  | otherwise         = ((0,1,1), (1,1,1,1), \_ [pc] [m] -> ([arg m pc], [op m pc], [pc+1], [m]))             -- no branch

-- | 'slice' takes a string containing one assembly command and slice it into
-- one string with the opcode and a list of Ints containing the arguments
slice :: String -> (String, [Int])
slice a = (head $ words a, args)
  where rest = tail $ words a
        args
          | null rest = []
          | length rest == 1 = [read (head rest) :: Int]
          | length rest == 2 = [read (head rest) :: Int, read (last rest) :: Int]
          | otherwise = error "slice: Some instruction has more than 2 arguments"

arg :: (Eq a, Num a) => Vector String -> a -> [Int]
arg x y = snd $ slice (atV x y)

op :: (Eq a, Num a) => Vector String -> a -> String
op x y = fst $ slice (atV x y)

-- | 'ifKernel' is the Instruction Fetch (IF) kernel process
ifKernel :: Num b => Signal ((Int, Int, Int), (Int, Int, Int, Int), [a] -> [b]
         -> [Vector String] -> ([d], [e], [b], [Vector String])) -> Signal a
         -> (Signal d, Signal e)
ifKernel ifCt sigBr = (sigArg, sigOp)
  where (sigArg, sigOp, sigPc, sigPm) = kernel34SADF ifCt sigBr sigPc' sigPm'
        sigPc' = delaySADF [0] sigPc
        sigPm' = delaySADF [progV] sigPm


---------------------------------------------------------
-- Execute (EXE) Kernel
---------------------------------------------------------

-- | 'exeScenario' is the list of possible scenarios for the EXE kernel
exeScenario :: Int -> ((Int, Int, Int), (Int, Int, Int, Int), [[Int]]
            -> [Vector Int] -> [Vector Int] -> ([Int], [Int], [Vector Int], [Vector Int]))
exeScenario 0  = ((1,0,0), (0,0,0,0), \_ _ _ -> ([], [], [], []))                                                     -- nop
exeScenario 1  = ((1,1,1), (0,0,1,1), \[[rd, mn]] [r] [m] -> ([], [], [replaceV r rd (atV m mn)], [m]))               -- ld
exeScenario 2  = ((1,1,1), (0,0,1,1), \[[rd, rm]] [r] [m] -> ([], [], [replaceV r rd (atV m (atV r rm))], [m]))       -- ldr
exeScenario 3  = ((1,1,1), (0,0,1,1), \[[rs, mn]] [r] [m] -> ([], [], [r], [replaceV m mn (atV r rs)]))               -- st
exeScenario 4  = ((1,1,1), (0,0,1,1), \[[rs, rm]] [r] [m] -> ([], [], [r], [replaceV m (atV r rm) (atV r rs)]))       -- str
exeScenario 5  = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd (atV r rs)], []))                  -- mov
exeScenario 6  = ((1,1,0), (0,0,1,0), \[[rd, i]] [r] _ -> ([], [], [replaceV r rd i], []))                            -- movi
exeScenario 7  = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) + (atV r rs))], []))   -- add
exeScenario 8  = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) - (atV r rs))], []))   -- sub
exeScenario 9  = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) * (atV r rs))], []))   -- mul
exeScenario 10 = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd (div (atV r rd) (atV r rs))], [])) -- div
exeScenario 11 = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) .&. (atV r rs))], [])) -- and
exeScenario 12 = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) .|. (atV r rs))], [])) -- or
exeScenario 13 = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd (xor (atV r rd) (atV r rs))], [])) -- xor
exeScenario 14 = ((1,1,0), (1,0,1,0), \[[rs, v]] [r] _ -> ([if atV r rs == 0 then v else 0], [], [r], []))            -- bez
exeScenario 15 = ((1,1,0), (1,0,1,0), \[[rs, v]] [r] _ -> ([if atV r rs /= 0 then v else 0], [], [r], []))            -- bnz
exeScenario 16 = ((1,1,0), (1,0,1,0), \[[rs, v]] [r] _ -> ([if atV r rs > 0 then v else 0], [], [r], []))             -- bgz
exeScenario 17 = ((1,1,0), (1,0,1,0), \[[rs, v]] [r] _ -> ([if atV r rs < 0 then v else 0], [], [r], []))             -- blz
exeScenario 18 = ((1,0,0), (1,0,0,0), \[[v]] _ _ -> ([v], [], [], []))                                                -- jmp
exeScenario 19 = ((1,1,0), (0,1,1,0), \[[rs]] [r] _ -> ([], [atV r rs], [r], []))                                     -- outr
exeScenario 20 = ((1,0,1), (0,1,0,1), \[[mn]] _ [m] -> ([], [atV m mn], [], [m]))                                     -- outm
exeScenario _  = error "exeScenario: Non existent scenario"

-- | 'exeKernel' is the Execution (EXE) kernel process
exeKernel :: Signal ((Int, Int, Int), (Int, Int, Int, Int), [a]
          -> [Vector Int] -> [Vector Int] -> ([d], [e], [Vector Int], [Vector Int]))
          -> Signal a -> (Signal d, Signal e)
exeKernel exeCt sigArg = (sigBr, sigDmp)
  where (sigBr, sigDmp, sigReg, sigDm) = kernel34SADF exeCt sigArg sigReg' sigDm'
        sigReg' = delaySADF [regV] sigReg
        sigDm' = delaySADF [memV] sigDm

regV = vector $ replicate 32 0
memV = vector $ replicate 1024 0

---------------------------------------------------------
-- Decode Detector
---------------------------------------------------------

-- | 'detectorScenario' is th output function of the detector. It converts a
-- state into an equivalent scenario
decScenario :: Int -> ((Int,Int), ([((Int, Int, Int), (Int, Int, Int, Int), [Int] -> [Int]
                 -> [Vector String] -> ([[Int]], [String], [Int], [Vector String]))],
                 [((Int, Int, Int), (Int, Int, Int, Int), [[Int]] -> [Vector Int]
                 -> [Vector Int] -> ([Int], [Int], [Vector Int], [Vector Int]))]))
decScenario n = ((1,1), ([ifScenario n], [exeScenario n]))

-- | 'decSwitchState' is the state transition function of the detector
decSwitchState :: Int -> [String] -> Int
decSwitchState _ ["nop"]  = 0
decSwitchState _ ["ld"]   = 1
decSwitchState _ ["ldr"]  = 2
decSwitchState _ ["st"]   = 3
decSwitchState _ ["str"]  = 4
decSwitchState _ ["mov"]  = 5
decSwitchState _ ["movi"] = 6
decSwitchState _ ["add"]  = 7
decSwitchState _ ["sub"]  = 8
decSwitchState _ ["mul"]  = 9
decSwitchState _ ["div"]  = 10
decSwitchState _ ["and"]  = 11
decSwitchState _ ["or"]   = 12
decSwitchState _ ["xor"]  = 13
decSwitchState _ ["bez"]  = 14
decSwitchState _ ["bnz"]  = 15
decSwitchState _ ["bgz"]  = 16
decSwitchState _ ["blz"]  = 17
decSwitchState _ ["jmp"]  = 18
decSwitchState _ ["outr"] = 19
decSwitchState _ ["outm"] = 20
decSwitchState _ _ = error "decSwitchState: Input not recognized"

-- | 'decodeDetector' is the detector of ProSyDe
decDetector :: Signal String -> (Signal ((Int, Int, Int), (Int, Int, Int, Int), [Int]
               -> [Int] -> [Vector String] -> ([[Int]], [String], [Int], [Vector String])),
               Signal ((Int, Int, Int), (Int, Int, Int, Int), [[Int]] -> [Vector Int]
               -> [Vector Int] -> ([Int], [Int], [Vector Int], [Vector Int])))
decDetector = detector12SADF 1 decSwitchState decScenario 0


---------------------------------------------------------
-- ProSyDe compact
---------------------------------------------------------

-- | 'prosyde' is the processor built with the ForSyDe SADF library. It outputs
-- whatever it is in register @rs@ during the execution of instruction @outr rs@
procNet :: Signal Int
procNet = sigDmp
  where (sigBr, sigDmp) = exeKernel exeCt sigArg
        (sigArg, sigOp) = ifKernel ifCt' sigBr
        (ifCt, exeCt) = decDetector sigOp
        ifCt' = delaySADF [(ifScenario 0)] ifCt


---------------------------------------------------------
-- Program Code
---------------------------------------------------------

progV :: Vector String
progV = vector [
  "movi 0 1",   -- reg 0 = 1
  "movi 1 1",   -- reg 1 = 1
  "movi 2 100", -- reg 2 = 100
  "add 0 1",    -- reg 0 = reg 0 + reg 1
  "sub 2 0",    -- reg 2 = reg 2 - reg 0
  "bez 2 1",    -- skip next instruction if reg 2 == 0
  "jmp -5",     -- jump back to instruction 3: "movi 2 100"
  "str 0 0",    -- mem(reg 0) = reg 0
  "ldr 10 0",   -- reg 10 = mem(reg 0)
  "outr 10",    -- output reg 10
  "outr 1",     -- output reg 1
  "outm 100",   -- output mem(100)
  "movi 10 7",  -- reg 10 = 7
  "st 10 42",   -- mem(42) = reg 10
  "outm 42",    -- output mem(42)
  "jmp -1"      -- end of program
  ]

-- progV :: Vector String
-- progV = vector [
--   "movi 0 3",
--   "st 0 0",
--   "movi 0 6",
--   "st 0 1",
--   "movi 0 7",
--   "st 0 2",
--   "movi 0 1",
--   "st 0 100",
--   "movi 0 8",
--   "st 0 101",
--   "movi 0 2",
--   "st 0 102",
--   "movi 0 0",   -- a pointer
--   "movi 1 100", -- b pointer
--   "movi 5 0",   -- result
--   "movi 6 1",   -- increment = 1
--   "movi 2 3",   -- lenth of a and b
--   "ldr 3 0",
--   "ldr 4 1",
--   "mul 3 4",
--   "add 5 3",
--   "add 0 6",    -- increment pointer of a
--   "add 1 6",    -- increment pointer of b
--   "sub 2 0",
--   "bez 2 1",
--   "jmp -10",
--   "outr 5"     -- display result
--   ]
