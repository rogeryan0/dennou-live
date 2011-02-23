# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	w_zonal_test_base.out \
	w_zonal_test_deriv.out w_zonal_test_integral.out \
	w_zonal_test_interpolate.out w_zonal_module_spectrum_test.out
	./w_zonal_test_base.out
	./w_zonal_test_deriv.out
	./w_zonal_test_integral.out
	./w_zonal_test_interpolate.out
	./w_zonal_module_spectrum_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

