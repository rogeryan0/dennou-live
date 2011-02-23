# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	ee_module_fftj_base_test1.out \
	ee_module_fftj_base_test2.out \
	ee_module_fftj_deriv_test1.out \
	ee_module_fftj_deriv_test2.out \
	ee_module_fftj_jacobian_test1.out \
	ee_module_fftj_jacobian_test2.out \
	ee_module_fftj_jacobianZ_test1.out \
	ee_module_fftj_jacobianZ_test2.out \
	ee_module_fftj_integral_test1.out \
	ee_module_fftj_integral_test2.out \
	ee_module_fftj_spectrum_test1.out \
	ee_module_fftj_spectrum_test2.out \
	ee_module_fftj_interpolate_test.out
	./ee_module_fftj_base_test1.out 
	./ee_module_fftj_base_test2.out 
	./ee_module_fftj_deriv_test1.out
	./ee_module_fftj_deriv_test2.out 
	./ee_module_fftj_jacobian_test1.out
	./ee_module_fftj_jacobian_test2.out
	./ee_module_fftj_jacobianZ_test1.out
	./ee_module_fftj_jacobianZ_test2.out
	./ee_module_fftj_integral_test1.out
	./ee_module_fftj_integral_test2.out
	./ee_module_fftj_spectrum_test1.out
	./ee_module_fftj_spectrum_test2.out
	./ee_module_fftj_interpolate_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
