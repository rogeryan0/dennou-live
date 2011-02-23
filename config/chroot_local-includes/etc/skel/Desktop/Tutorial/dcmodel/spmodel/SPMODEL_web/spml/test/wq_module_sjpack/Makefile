# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc *.mod

test:	wq_module_sjpack_base_test.out \
	wq_module_sjpack_deriv1_test.out \
	wq_module_sjpack_deriv2_test.out \
	wq_module_sjpack_integral_test.out \
	wq_module_sjpack_spectrum_test.out \
	wq_module_sjpack_torvelbc_test.out \
	wq_module_sjpack_polvelbc_test.out \
	wq_module_sjpack_tormagbc_test.out \
	wq_module_sjpack_polmagbc_test.out 
	./wq_module_sjpack_base_test.out 
	./wq_module_sjpack_deriv1_test.out 
	./wq_module_sjpack_deriv2_test.out 
	./wq_module_sjpack_integral_test.out 
	./wq_module_sjpack_spectrum_test.out 
	./wq_module_sjpack_torvelbc_test.out 
	./wq_module_sjpack_polvelbc_test.out 
	./wq_module_sjpack_tormagbc_test.out 
	./wq_module_sjpack_polmagbc_test.out 

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

