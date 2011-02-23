# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc *.mod

.SUFFIXES: .f90 .out

test:	eq_test_transform.out eq_test_derivative.out \
	eq_test_jacobian.out eq_test_LaplaInv.out \
	eq_test_boundary.out eq_test_Vor2Strum.out eq_test_intavr.out 
	./eq_test_transform.out
	./eq_test_derivative.out
	./eq_test_jacobian.out
	./eq_test_boundary.out
	./eq_test_LaplaInv.out
	./eq_test_Vor2Strum.out
	./eq_test_intavr.out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
