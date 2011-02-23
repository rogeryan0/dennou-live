# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	au_test_base1d.out au_test_base2d.out \
	au_test_intavr.out au_test_weight.out \
	au_test_boundary_grid_1d.out au_test_boundary_grid_2d.out \
	au_test_boundary_tau_1d.out au_test_boundary_tau_2d.out
	./au_test_base1d.out
	./au_test_base2d.out
	./au_test_intavr.out
	./au_test_weight.out
	./au_test_boundary_tau_1d.out
	./au_test_boundary_tau_2d.out
	./au_test_boundary_tau_1d.out
	./au_test_boundary_tau_2d.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

