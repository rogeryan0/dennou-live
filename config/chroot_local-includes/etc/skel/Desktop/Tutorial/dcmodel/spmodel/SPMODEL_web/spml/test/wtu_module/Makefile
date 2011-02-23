# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

test:	wtu_test_base.out \
	wtu_test_derivative1.out wtu_test_derivative2.out \
	wtu_test_derivative3.out wtu_test_derivative4.out \
	wtu_test_derivative5.out wtu_test_derivative7.out \
	wtu_test_derivative11.out wtu_test_derivative12.out \
	wtu_test_derivative13.out wtu_test_derivative14.out \
	wtu_test_derivative15.out wtu_test_derivative17.out \
	wtu_test_polvelbci.out wtu_test_torvelbci.out \
	wtu_test_polvelbco.out wtu_test_torvelbco.out \
	wtu_test_polmagbci.out wtu_test_tormagbci.out \
	wtu_test_polmagbco.out wtu_test_tormagbco.out \
	wtu_test_polmagbctau.out wtu_test_tormagbctau.out \
	wtu_test_polmagbcgrid.out wtu_test_tormagbcgrid.out
	./wtu_test_base.out 
	./wtu_test_derivative1.out 
	./wtu_test_derivative2.out 
	./wtu_test_derivative3.out 
	./wtu_test_derivative4.out 
	./wtu_test_derivative5.out 
	./wtu_test_derivative7.out 
	./wtu_test_derivative11.out 
	./wtu_test_derivative12.out 
	./wtu_test_derivative13.out 
	./wtu_test_derivative14.out 
	./wtu_test_derivative15.out 
	./wtu_test_derivative17.out 
	./wtu_test_polvelbci.out
	./wtu_test_torvelbci.out
	./wtu_test_polvelbco.out
	./wtu_test_torvelbco.out
	./wtu_test_polmagbci.out
	./wtu_test_tormagbci.out
	./wtu_test_polmagbco.out
	./wtu_test_tormagbco.out
	./wtu_test_polmagbctau.out
	./wtu_test_tormagbctau.out
	./wtu_test_polmagbcgrid.out
	./wtu_test_tormagbcgrid.out

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
