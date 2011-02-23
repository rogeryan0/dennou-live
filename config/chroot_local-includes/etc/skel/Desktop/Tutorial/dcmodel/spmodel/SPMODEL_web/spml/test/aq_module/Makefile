# Makefile for spmodel library
#
# $Id: Makefile,v 1.4 2009-07-24 09:19:53 uwabami Exp $
# 


include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc *.mod

test:	aq_test_base1d.out aq_test_base2d.out  \
	aq_test_weight.out aq_test_intavr.out  \
	aq_test_boundary_tau_1d.out aq_test_boundary_tau_2d.out \
	aq_test_boundary_grid_1d.out aq_test_boundary_grid_2d.out 
	./aq_test_base1d.out
	./aq_test_base2d.out
	./aq_test_weight.out
	./aq_test_intavr.out
	./aq_test_boundary_tau_1d.out
	./aq_test_boundary_tau_2d.out
	./aq_test_boundary_grid_1d.out
	./aq_test_boundary_grid_2d.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

