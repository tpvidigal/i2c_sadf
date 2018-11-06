## VARIABLES ############################

PATH_SOURCE=../src
TESTBENCH=

## TESTBENCHES ##########################

run_start:
	$(MAKE) run TESTBENCH=Start

## MACRO FUNCTIONS ######################

run:
	runhaskell -i$(PATH_SOURCE) I2CSlave$(TESTBENCH)_tb.hs

