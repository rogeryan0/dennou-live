# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	wa_base_module_sjpack_test.out wa_deriv_module_sjpack_test.out \
	wa_integral_module_sjpack_test.out \
	wa_interpolate_module_sjpack_test.out wa_spectrum_module_sjpack_test.out
	./wa_base_module_sjpack_test.out
	./wa_deriv_module_sjpack_test.out
	./wa_integral_module_sjpack_test.out
	./wa_interpolate_module_sjpack_test.out
	./wa_spectrum_module_sjpack_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

