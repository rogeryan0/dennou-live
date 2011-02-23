# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

.SUFFIXES: .f90 .out

test:	et_test_transform.out et_test_derivative.out \
	et_test_jacobian.out et_test_LaplaInv.out \
	et_test_boundaries.out et_test_Vor2Strum.out et_test_intavr.out 
	./et_test_transform.out
	./et_test_derivative.out
	./et_test_jacobian.out
	./et_test_boundaries.out
	./et_test_LaplaInv.out
	./et_test_Vor2Strum.out
	./et_test_intavr.out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
