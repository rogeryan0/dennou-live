# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	wt_module_sjpack_base_test.out \
	wt_module_sjpack_deriv_test1.out \
	wt_module_sjpack_deriv_test2.out \
	wt_module_sjpack_integral_test.out \
	wt_module_sjpack_interpolate_test.out \
	wt_module_sjpack_spectrum_test.out \
	wt_module_sjpack_polvelbc_test.out \
	wt_module_sjpack_torvelbc_test.out \
	wt_module_sjpack_polmagbc_test.out \
	wt_module_sjpack_tormagbc_test.out 
	./wt_module_sjpack_base_test.out
	./wt_module_sjpack_deriv_test1.out
	./wt_module_sjpack_deriv_test2.out
	./wt_module_sjpack_integral_test.out
	./wt_module_sjpack_interpolate_test.out
	./wt_module_sjpack_spectrum_test.out
	./wt_module_sjpack_polvelbc_test.out
	./wt_module_sjpack_torvelbc_test.out
	./wt_module_sjpack_polmagbc_test.out
	./wt_module_sjpack_tormagbc_test.out 

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

