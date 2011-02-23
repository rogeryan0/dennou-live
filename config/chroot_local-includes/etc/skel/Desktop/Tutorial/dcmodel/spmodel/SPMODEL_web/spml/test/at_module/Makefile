# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	at_test_base1d.out at_test_base2d.out at_test_base.out \
	at_test_intavr.out at_test_interpolate.out at_test_weight.out \
	at_test_boundaries1d.out at_test_boundaries2d.out
	./at_test_base1d.out
	./at_test_base2d.out
	./at_test_base.out
	./at_test_intavr.out
	./at_test_interpolate.out
	./at_test_weight.out
	./at_test_boundaries1d.out
	./at_test_boundaries2d.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

