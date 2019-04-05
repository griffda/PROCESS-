.PHONY: tests clean all
.DEFAULT_GOAL = tests

# Determine operating system, architecture and compiler
# automatically if possible
UNAME ?=$(shell uname)
ifeq ($(UNAME),)
	UNAME =UNKNOWN
else
# Check for Windows/CYGWIN compilation.
ifneq (,$(findstring CYGWIN,$(UNAME)))
	UNAME =Windows
endif
endif

ifneq ($(UNAME),Windows)
	TOP_DIR := $(shell pwd)
	SO_DIR=$(TOP_DIR)/bin
	SRC_DIR=$(TOP_DIR)/src
	BLD_DIR=$(TOP_DIR)/build
	PRC_DIR=$(TOP_DIR)/build/CMakeFiles/process.exe.dir/source/fortran
	PRC_CALC_DIR=$(TOP_DIR)/build/CMakeFiles/PROCESS_calc_engine.dir/source/fortran
	PLSM_DIR=$(TOP_DIR)/build/CMakeFiles/process.exe.dir/lib/PLASMOD
	TEST_DIR=$(TOP_DIR)/test_files/pfunit_files
	export LD_LIBRARY_PATH=$(SO_DIR)
else
	# When using CYGWIN, then relative paths have to be used.
	PFUNIT := ../../../pfunit
	TOP_DIR := $(shell pwd)
	SRC_DIR=src
	TEST_DIR=tests
endif

VPATH = . $(SRC_DIR) $(TEST_DIR)

include $(PFUNIT)/include/base.mk

ifeq ($(UNAME),Windows)
	FFLAGS += -DWindows
	ifeq ($(FC),ifort)
		FFLAGS += /nologo
	endif
endif

# The following may be redundant since FC should already be
# appropriately set in include/base.mk.
ifeq ($(USEMPI),YES)
   FC=mpif90
endif

EXE = tests$(EXE_EXT)
ifneq ($(UNAME),Windows)
	LIBS = -L$(PFUNIT)/lib -lpfunit 
else
	LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)
endif

all: $(EXE)

# ifeq ($(USEMPI),YES)
# 	mpirun -np 1 ./$(EXE)
# else
# 	./$(EXE)
# endif

ifeq ($(USEMPI),YES)
	mpirun -np 1 ./$(EXE) -xml pfunit_results.xml
else
	./$(EXE) -xml pfunit_results.xml
endif

SUT: $(PRC_CALC_DIR)
	rm -f $(TEST_DIR)/*.mod
	rm -f $(TEST_DIR)/*.o
	rm -f $(PRC_DIR)/process.f90.o
	make -C $(TEST_DIR) tests

tests: all

$(EXE): testSuites.inc SUT
	$(FC) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -I$(TEST_DIR) $(PFUNIT)/include/driver.F90 $(SO_DIR)/*.so $(PRC_CALC_DIR)/*$(OBJ_EXT) $(TEST_DIR)/*$(OBJ_EXT) $(LIBS) $(FFLAGS) $(FPPFLAGS)
#	$(FC) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -Itests $(PFUNIT)/include/driver.F90 $(TEST_DIR)/*$(OBJ_EXT) $(PRC_DIR)/*$(OBJ_EXT) $(PLSM_DIR)/*$(OBJ_EXT) $(LIBS) $(FFLAGS) $(FPPFLAGS)

distclean: clean

clean: local-E0-clean

local-E0-clean:
	rm -f $(TEST_DIR)/*.mod $(TEST_DIR)/*.o
#	make -C $(SRC_DIR) clean
#	make -C $(TEST_DIR) clean
	rm -f $(EXE) *$(OBJ_EXT) tests.xml

ifeq ($(UNAME),Windows)
	export PFUNIT
endif

export FC
export FPPFLAGS
export FFLAGS
export TEST_DIR
export BLD_DIR
export PRC_DIR
export PRC_CALC_DIR
export OBJ_EXT
export LIB_EXT
export EXE_EXT

