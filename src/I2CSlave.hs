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
-- * There are 2 kernels responsible for condition detection. They always
--   operate the same way (no detector necessary) and all other processes will
--   monitor those signals
--   > START condition monitor (kernelStart)
--   > STOP condition monitor (kernelStop)
--
-- * There is 1 kernel similar to the condition ones. However, it acts like
--   a synchronizer for I2C data. When a positive edge at the SCL line is
--   detected, it generates a token with the SDA value sampled. In other
--   words, it's the clock of the system, with the SDA value as a bonus.
--   > Data value at posedge (kernelDataPosedge)
--
-- * There is 1 kernel to monitor the address being transmitted after a START
--   condition. If the 7 bits received match the slave address, the 8th bit
--   is send as a token and determines the operation that will be performed.
--   > Address Monitor (kernelAddressMonitor)
--   > Address Monitor controller (detectAddressMonitor)
--
-- * There is 1 detector that will coordiante the operations execution. When
--   operation to execute is determined (Master read or write), this detector
--   will manage the scenarios that the operation kernels must be at each
--   step of the execution.
--   > Operation Controller (detectOpControl)
--
-- * There will be 2 operation kernels. Each of them will contain the steps
--   for the Master read or write operation. They are also responsible to
--   define the SDA value of slave to control acknowledge and data signals.
--   > Master read operator (kernelOpRead)
--   > Master write operator (kernelOpWrite)
--
-- * There will be 1 last kernel to control the slave's SDA line. It monitors
--   the operation kernels all the time. If one of them push SDA to 0, it
--   defines the final Slave's SDA value to 0. It basically centralizes the
--   required SDA changes. It's scenario is defined by the operations controller.
--   > Manager of SDA line (kernelSDAManager)
--
-----------------------------------------------------------------------------

module I2CSlave (

  -- Simulate I2C Slave model using the given stimulus
  simulate,

  -- I2C Slave block
  i2cSlave

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

-- Conditions
-- start                        = kernelConditStart twoLines
-- stop                         = kernelConditStop  twoLines

-- Clock
-- sdaPosedge                   = kernelDataPosedge twoLines

-- Address
-- (fbAddress,readOp)           = kernelAddressMonitor ctrlAddress twoLines
-- ctrlAddress = detectAddressMonitor sdaPosedge fbAddress (start,stop)

-- Controller
-- (ctrlRead,ctrlWrite,ctrlSDA) = detectOpControl sdaPosedge (start,stop) fbRead fbWrite keepRead readOp

-- Operations
-- (fbRead,byteRead,sdaRead)    = kernelOpRead  ctrlRead  sdaPosedge
-- (fbWrite,doneWrite,sdaWrite) = kernelOpWrite ctrlWrite sdaPosedge

-- SDA output of slave
-- sdaOut                       = kernelManagerSDA ctrlSDA sdaOpRead sdaOpWrite



