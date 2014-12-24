# Copyright (c) 2012-2015 Bluespec, Inc.,  All Rights Reserved

# Assumes BCLIB_OBJ_BSIM defined in environment or by parent make

BUILD_DIR = ../build_bsim
BCLIB_OBJ_BSIM = ../../BClib_obj_bsim/
TARGET ?= SIM

CC = cc
CFLAGS = -fPIC -O3 -DFOR_$(TARGET) -D_FILE_OFFSET_BITS=64  -I$(BCLIB_OBJ_BSIM)

OBJECTS = $(BUILD_DIR)/App_SW.o $(BUILD_DIR)/timing.o  $(BUILD_DIR)/instrumentation.o

# ----------------------------------------------------------------

.PHONY: default
default: $(OBJECTS)

# ----------------------------------------------------------------

$(BUILD_DIR)/App_SW.o: App_SW.cpp  timing.h  instrumentation.h
	$(CC)  -c  $(CFLAGS)  -o $(BUILD_DIR)/App_SW.o  App_SW.cpp

# ----------------------------------------------------------------

$(BUILD_DIR)/timing.o: timing.h timing.cpp
	$(CC)  -c  $(CFLAGS)  -o $(BUILD_DIR)/timing.o  timing.cpp

# ----------------------------------------------------------------

$(BUILD_DIR)/instrumentation.o: instrumentation.h instrumentation.c
	$(CC)  -c  $(CFLAGS)  -o $(BUILD_DIR)/instrumentation.o  instrumentation.c

# ----------------------------------------------------------------
.PHONY: clean
clean:
	rm -f  *~  *.o
