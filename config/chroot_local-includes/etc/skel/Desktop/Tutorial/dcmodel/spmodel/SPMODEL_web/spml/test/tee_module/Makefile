# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

FFLAGS=-Hausex

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	tee_module_base_test.out \
	tee_module_deriv_test.out \
	tee_module_integral_test.out \
	tee_module_interpolate_test.out \
	tee_module_torpol_test.out  \
	tee_module_spectrum_test.out \
	tee_module_BoundariesTau_test.out  \
	tee_module_BoundariesGrid_test.out \
	tee_module_TorvelbcTau_test.out  \
	tee_module_TorvelbcGrid_test.out \
	tee_module_PolvelbcGrid_test.out \
	tee_module_PolvelbcCheb_test.out \
	tee_module_TormagbcTau_test.out  \
	tee_module_TormagbcGrid_test.out \
	tee_module_PolmagbcTau_test.out  \
	tee_module_PolmagbcGrid_test.out
	./tee_module_base_test.out
	./tee_module_deriv_test.out
	./tee_module_integral_test.out
	./tee_module_interpolate_test.out
	./tee_module_torpol_test.out
	./tee_module_spectrum_test.out
	./tee_module_BoundariesTau_test.out
	./tee_module_BoundariesGrid_test.out
	./tee_module_TorvelbcTau_test.out
	./tee_module_TorvelbcGrid_test.out
	./tee_module_PolvelbcGrid_test.out
	./tee_module_PolvelbcCheb_test.out
	./tee_module_TormagbcTau_test.out
	./tee_module_TormagbcGrid_test.out
	./tee_module_PolmagbcTau_test.out
	./tee_module_PolmagbcGrid_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(FFLAGS) $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
