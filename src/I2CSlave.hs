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
-- * There will be 1 kernel similar to the condition ones. However, it acts
--   like a synchronizer for I2C data. When a positive edge at the SCL line
--   is detected, it generates a token with the SDA value sampled
--   > Data value at posedge (DataPosedge)
--
-- * There will be 3 operation kernels. Each one requires a detector to model
--   the FSM they must obey. Also, they watch conditions to reset or start
--   their operation. Finally, they define the SDA value of slave to control
--   acknowledge and data signals.
--   > Address + read/write selector monitor (OpAddress)
--   > Master read operator (OpRead)
--   > Master write operator (OpWrite)
--
-- * There will be 1 last kernel to control the slave's SDA line. It monitors
--   the operation kernels all the time. If one of them push SDA to 0, it
--   defines the final Slave's SDA value to 0. It basically centralizes the
--   required SDA changes.
--   > Manager of SDA line (ManagerSDA)
--
-----------------------------------------------------------------------------

module I2CSlave (

  -- Simulate I2C Slave model using the given stimulus
  simulate

) where

import ForSyDe.Shallow
import SADF
import Data.Bits((.&.), (.|.), xor)

import I2CSlaveGlobals
import I2CSlaveStart
import I2CSlaveStop
import I2CSlavePosedge
import I2CSlaveOpAddress



--------------------------------------------------------
-- System
---------------------------------------------------------

-- start                               = kernelConditStart twoLines
-- stop                                = kernelConditStop  twoLines
-- sdaPosedge                          = kernelDataPosedge twoLines
-- (fbOpAddress, readOp, sdaOpAddress) = kernelOpAddress   ctrlOpAddress twoLines (start,stop)
-- (fbOpRead, sdaOpRead)               = kernelOpRead      ctrlOpRead    twoLines (start,stop)
-- (fbOpWrite, sdaOpWrite)             = kernelOpWrite     ctrlOpWrite   twoLines (start,stop)
-- sdaOut                              = kernelManagerSDA  sdaOpAddress sdaOpRead sdaOpWrite



