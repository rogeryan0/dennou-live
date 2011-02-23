# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	l_nm_test.out nm_l_test.out \
	w_test_base.out w_test_deriv.out w_test_integral.out \
	w_test_interpolate00.out w_spectrum_module_test.out
	./l_nm_test.out
	./nm_l_test.out
	./w_test_base.out
	./w_test_deriv.out
	./w_test_integral.out
	./w_test_interpolate00.out
	./w_spectrum_module_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

