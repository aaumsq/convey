# Copyright (c) 2012-2015 Bluespec, Inc., All Rights Reserved
# Distributed under license

# This Makefile is for pre-compiling the BClib library after the
# sources have been installed in this directory.
# I.e., it is only done once, as part of installation, after which
# applications just use the pre-compiled object and verilog files.
# For Bluesim, it leaves it leaves
#   include files (.h) and object files (.o, .bo, .ba) in ../BClib_obj_bsim
# For Verilog, it leaves it leaves
#   object file (.o, .bo, .ba) in ../BClib_obj_v
#   and Verilog files (.v) in ../BClib_verilog

BCLIB_OBJ_BSIM = ../BClib_obj_bsim
BCLIB_OBJ_V    = ../BClib_obj_v
BCLIB_VDIR     = ../BClib_verilog

.PHONY: default
default:
	@echo "INFO: Creating directories $(BCLIB_OBJ_BSIM), $(BCLIB_OBJ_V)  and  $(BCLIB_VDIR)"
	mkdir -p  $(BCLIB_OBJ_BSIM)  $(BCLIB_OBJ_V)  $(BCLIB_VDIR)
	@echo "INFO: Compiling BClib BSV files"
	make  default_BSV
	@echo "INFO: Compiling BClib C files"
	make  default_C
	@echo "INFO: Completed BClib build"

include  Makefile_BSV  Makefile_C

# ----------------------------------------------------------------

.PHONY: clean
clean:
	rm -r -f  *~  $(BCLIB_OBJ_BSIM)/*  $(BCLIB_OBJ_V)/*  $(BCLIB_VDIR)/*

.PHONY: full_clean
full_clean:
	make clean
