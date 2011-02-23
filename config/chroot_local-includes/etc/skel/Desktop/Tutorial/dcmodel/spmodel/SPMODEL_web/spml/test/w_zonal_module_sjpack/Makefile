# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	w_zonal_module_sjpack_base_test.out \
	w_zonal_module_sjpack_deriv_test.out \
	w_zonal_module_sjpack_integral_test.out \
	w_zonal_module_sjpack_interpolate_test.out \
	w_zonal_module_sjpack_spectrum_test.out
	./w_zonal_module_sjpack_base_test.out
	./w_zonal_module_sjpack_deriv_test.out
	./w_zonal_module_sjpack_integral_test.out
	./w_zonal_module_sjpack_interpolate_test.out
	./w_zonal_module_sjpack_spectrum_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

