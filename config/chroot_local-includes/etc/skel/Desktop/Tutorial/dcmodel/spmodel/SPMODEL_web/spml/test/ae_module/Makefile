# Makefile for spmodel library
#
# $Id: Makefile,v 1.5 2009-01-16 16:23:04 uwabami Exp $
# 

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	ae_test_base1d.out ae_test_base2d.out ae_test_intavr.out 
	./ae_test_base1d.out
	./ae_test_base2d.out
	./ae_test_intavr.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
