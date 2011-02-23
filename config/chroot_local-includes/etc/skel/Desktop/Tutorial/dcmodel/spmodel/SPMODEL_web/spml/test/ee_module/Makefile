# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	ee_test_transform.out ee_test_derivative.out \
	ee_test_jacobian.out ee_test_jacobianZ.out \
	ee_test_intavr.out ee_test_spectrum.out \
	ee_test_transform2.out ee_test_derivative2.out \
	ee_test_jacobian2.out ee_test_jacobianZ2.out \
	ee_test_intavr2.out ee_test_spectrum2.out \
	ee_test_interpolate.out
	./ee_test_transform.out 
	./ee_test_derivative.out 
	./ee_test_jacobian.out
	./ee_test_jacobianZ.out
	./ee_test_intavr.out
	./ee_test_spectrum.out
	./ee_test_transform2.out 
	./ee_test_derivative2.out 
	./ee_test_jacobian2.out
	./ee_test_jacobianZ2.out
	./ee_test_intavr2.out
	./ee_test_spectrum2.out
	./ee_test_interpolate.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
