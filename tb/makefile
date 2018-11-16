## VARIABLES ############################

PATH_SOURCE=../src
TESTBENCH=

## TESTBENCHES ##########################

run_start:
	@$(MAKE) run TESTBENCH=Start

## MACRO FUNCTIONS ######################

run:
	@if [ ! -d ../log ]; then \
	    mkdir ../log; \
	fi;
	@runhaskell -i$(PATH_SOURCE) I2CSlave$(TESTBENCH)_tb.hs > ../log/run_$(TESTBENCH).log
	@cat ../log/run_$(TESTBENCH).log

report:
	@echo " "
	@echo "== Tests that PASSED =========="
	@echo " "
	@grep -r -l "PASSED" ../log || true
	@echo " "
	@echo "== Tests that FAILED =========="
	@echo " "
	@grep -r -l "FAILED" ../log || true
	@echo " "
	@echo "==============================="
	@echo " "

