# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	w_base_module_sjpack_test.out w_deriv_module_sjpack_test.out \
	w_integral_module_sjpack_test.out \
	w_interpolate_module_sjpack_test.out w_spectrum_module_sjpack_test.out
	./w_base_module_sjpack_test.out
	./w_deriv_module_sjpack_test.out
	./w_integral_module_sjpack_test.out
	./w_interpolate_module_sjpack_test.out
	./w_spectrum_module_sjpack_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

