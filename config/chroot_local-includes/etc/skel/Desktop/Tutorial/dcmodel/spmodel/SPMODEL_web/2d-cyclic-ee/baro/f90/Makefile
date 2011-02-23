# Makefile for spmodel 

#SPMFRT=spmfrt
#SPMFRT=/usr/bin/spmfrt.ifc6
#SPMFRT=/usr/bin/spmfrt.ffc
SPMFRT=/usr/bin/spmfrt.ssl2-ffc5
#MODS=*.d
MODS=*.mod

FFLAGS=-H aesux

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out 

clean.all:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc *.log *.nml

.SUFFIXES: .f90 .out

.f90.out:
	$(SPMFRT) $(FFLAGS) $< -o $@
