# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

test:	wt_test_base.out \
	wt_test_derivative1.out wt_test_derivative2.out \
	wt_test_derivative3.out wt_test_derivative4.out \
	wt_test_derivative5.out wt_test_derivative6.out \
	wt_test_derivative7.out wt_test_derivative8.out \
	wt_test_intavr.out \
	wt_test_interpolate.out \
	wt_module_spectrum_test.out \
	wt_test_polvelbc.out wt_test_torvelbc.out \
	wt_test_polmagbc.out wt_test_tormagbc.out
	./wt_test_base.out 
	./wt_test_derivative1.out 
	./wt_test_derivative2.out 
	./wt_test_derivative3.out 
	./wt_test_derivative4.out 
	./wt_test_derivative5.out 
	./wt_test_derivative6.out 
	./wt_test_derivative7.out 
	./wt_test_intavr.out 
	./wt_test_interpolate.out 
	./wt_module_spectrum_test.out
	./wt_test_polvelbc.out
	./wt_test_torvelbc.out
	./wt_test_polmagbc.out
	./wt_test_tormagbc.out

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

