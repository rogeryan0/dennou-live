# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	esc_test_transform.out esc_test_derivative.out \
	esc_test_jacobian.out esc_test_intavr.out 
	./esc_test_transform.out
	./esc_test_derivative.out
	./esc_test_jacobian.out
	./esc_test_intavr.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
