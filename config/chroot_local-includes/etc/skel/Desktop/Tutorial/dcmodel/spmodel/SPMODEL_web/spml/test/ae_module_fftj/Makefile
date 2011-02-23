# Makefile for spmodel library
#
# $Id: Makefile,v 1.1 2009-09-11 07:32:51 takepiro Exp $
# 

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	ae_module_fftj_base1d_test.out \
	ae_module_fftj_base2d_test.out \
	ae_module_fftj_integral_test.out
	./ae_module_fftj_base1d_test.out
	./ae_module_fftj_base2d_test.out
	./ae_module_fftj_integral_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
