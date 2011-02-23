# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	wa_zonal_test_base.out wa_zonal_test_deriv.out \
	wa_zonal_test_integral.out \
	wa_zonal_test_interpolate.out wa_zonal_module_spectrum_test.out
	./wa_zonal_test_base.out
	./wa_zonal_test_deriv.out
	./wa_zonal_test_integral.out
	./wa_zonal_test_interpolate.out
	./wa_zonal_module_spectrum_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

