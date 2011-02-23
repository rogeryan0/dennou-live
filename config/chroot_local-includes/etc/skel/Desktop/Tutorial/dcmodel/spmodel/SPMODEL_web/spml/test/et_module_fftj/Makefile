# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

.SUFFIXES: .f90 .out

test:	et_module_fftj_base_test.out \
	et_module_fftj_deriv_test.out \
	et_module_fftj_integral_test.out \
	et_module_fftj_jacobian_test.out \
	et_module_fftj_boundaries_test.out \
	et_module_fftj_Vor2Strum_test.out
	./et_module_fftj_base_test.out 
	./et_module_fftj_deriv_test.out 
	./et_module_fftj_integral_test.out 
	./et_module_fftj_jacobian_test.out 
	./et_module_fftj_boundaries_test.out
	./et_module_fftj_Vor2Strum_test.out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
