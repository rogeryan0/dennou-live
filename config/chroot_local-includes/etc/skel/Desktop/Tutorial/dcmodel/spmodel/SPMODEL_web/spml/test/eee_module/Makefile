# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	eee_test_ZetaVorVel.out   eee_test_intavr.out \
	eee_test_ZetaVorVel2.out  eee_test_intavr2.out \
	eee_test_ESpectral.out    eee_test_derivative.out \
	eee_test_ESpectral2.out   eee_test_derivative2.out \
	eee_test_RotVelxVor.out   eee_test_energyhelicity.out \
	eee_test_RotVelxVor2.out  eee_test_energyhelicity2.out \
	eee_test_transform.out    eee_test_transform2.out \
	./eee_test_ZetaVorVel.out
	./eee_test_intavr.out
	./eee_test_ZetaVorVel2.out
	./eee_test_ESpectral.out
	./eee_test_ESpectral2.out
	./eee_test_RotVelxVor.out
	./eee_test_RotVelxVor2.out
	./eee_test_transform.out
	./eee_test_intavr2.out
	./eee_test_derivative.out
	./eee_test_derivative2.out
	./eee_test_energyhelicity.out
	./eee_test_transform2.out
	./eee_test_energyhelicity2.out


.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
