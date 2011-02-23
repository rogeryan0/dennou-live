# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

test:	wu_test_base.out \
	wu_test_derivative1.out wu_test_derivative2.out \
	wu_test_derivative3.out wu_test_derivative4.out \
	wu_test_derivative5.out wu_test_derivative6.out \
	wu_test_derivative7.out \
	wu_test_intavr.out \
	wu_test_polvelbc.out wu_test_torvelbc.out \
	wu_test_polmagbc.out wu_test_tormagbc.out
	./wu_test_base.out 
	./wu_test_derivative1.out 
	./wu_test_derivative2.out 
	./wu_test_derivative3.out 
	./wu_test_derivative4.out 
	./wu_test_derivative5.out 
	./wu_test_derivative6.out 
	./wu_test_derivative7.out 
	./wu_test_intavr.out
	./wu_test_polvelbc.out
	./wu_test_torvelbc.out
	./wu_test_polmagbc.out
	./wu_test_tormagbc.out

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
