# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	wa_test_base.out wa_test_deriv.out wa_test_integral.out \
	wa_test_interpolate00.out wa_spectrum_module_test.out
	./wa_test_base.out
	./wa_test_deriv.out
	./wa_test_integral.out
	./wa_test_interpolate00.out
	./wa_spectrum_module_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

