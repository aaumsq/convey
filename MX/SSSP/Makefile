# Copyright (c) 2012-2015 Bluespec, Inc.,  All Rights Reserved

# Assumes BCLIB already defined in environment

export BCLIB_SRC      = $(BCLIB)/BClib_src
export BCLIB_OBJ_BSIM = $(BCLIB)/BClib_obj_bsim
export BCLIB_OBJ_V    = $(BCLIB)/BClib_obj_v
export BCLIB_VDIR     = $(BCLIB)/BClib_verilog

# For Linux
 LIBS	= -Xl -lpthread -Xl -lrt

# For OS X
#LIBS	= -Xl -lpthread

BSCDIRS_BSIM  = -simdir build_bsim  -bdir build_bsim  -info-dir build_bsim
BSCDIRS_V     = -vdir Personality/verilog  -bdir build_v  -info-dir build_v
BSCFLAGS = -aggressive-conditions -no-show-method-conf -no-warn-action-shadowing

C_OBJECTS = build_bsim/App_SW.o  \
            build_bsim/timing.o  \
            build_bsim/instrumentation.o  \
            $(BCLIB_OBJ_BSIM)/BC_linkage_Bluesim.o

default: compile link simulate

build_bsim:
	mkdir -p build_bsim
build_v:
	mkdir -p build_v

.PHONY: typecheck
typecheck: build_bsim
	@echo Typechecking...
	bsc -u $(BSCDIRS_BSIM) $(BSCFLAGS) -p .:./BSV_src:$(BCLIB_OBJ_BSIM):%/Prelude:%/Libraries \
	        $(BCLIB_SRC)/BC_Sim_Main.bsv
	@echo Typechecking finished

.PHONY: compile
compile: build_bsim
	@echo Compiling...
	cd C_src; make
	bsc -u -sim $(BSCDIRS_BSIM) $(BSCFLAGS) -p .:./BSV_src:$(BCLIB_OBJ_BSIM):%/Prelude:%/Libraries \
	        -g mkBC_Sim_Main  $(BCLIB_SRC)/BC_Sim_Main.bsv 
	@echo Compilation finished

.PHONY: link
link: build_bsim
	@echo Linking...
	bsc -e mkBC_Sim_Main -sim -o ./out $(BSCDIRS_BSIM) $(BSCFLAGS) \
	    -p .:$(BCLIB_OBJ_BSIM):%/Prelude:%/Libraries \
	    $(LIBS) $(C_OBJECTS)
	@echo Linking finished

.PHONY: simulate
simulate:
	@echo Simulation...
	./out | tee transcript
	@echo Simulation finished

# ----------------------------------------------------------------

.PHONY: verilog
verilog: build_v
	@echo Verilog generation ...
	bsc -u -verilog $(BSCDIRS_V) $(BSCFLAGS) -p .:./BSV_src:$(BCLIB_OBJ_V):%/Prelude:%/Libraries \
	    -g cae_pers  $(BCLIB_SRC)/Cae_Pers.bsv
	@echo Verilog generation finished

# ----------------------------------------------------------------

.PHONY: clean
clean:
	cd C_src; make clean
	cd Personality/verilog; make clean
	rm -r -f  *~  BSV_src/*~  C_src/*~  build_bsim/*  build_v/*

.PHONY: full_clean
full_clean:
	make clean
	rm -r -f  out  out.*  transcript  build_bsim  build_v
