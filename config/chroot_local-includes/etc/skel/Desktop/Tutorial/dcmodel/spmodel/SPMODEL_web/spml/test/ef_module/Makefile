# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	ef_module_base_test.out \
	ef_module_deriv_test.out \
	ef_module_integral_test.out \
	ef_module_interpolate_test.out 
	./ef_module_base_test.out 
	./ef_module_deriv_test.out
	./ef_module_integral_test.out  
	./ef_module_interpolate_test.out 

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
