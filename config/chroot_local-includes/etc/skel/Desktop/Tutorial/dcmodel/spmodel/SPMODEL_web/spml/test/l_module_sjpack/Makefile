# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	l_module_sjpack_base_test.out l_module_sjpack_deriv_test.out \
	l_module_sjpack_integral_test.out l_module_sjpack_interpolate_test.out
	./l_module_sjpack_base_test.out
	./l_module_sjpack_deriv_test.out
	./l_module_sjpack_integral_test.out
	./l_module_sjpack_interpolate_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

