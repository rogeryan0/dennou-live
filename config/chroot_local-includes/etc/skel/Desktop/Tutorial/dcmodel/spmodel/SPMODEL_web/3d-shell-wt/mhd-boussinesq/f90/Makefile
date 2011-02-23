# Makefile for spmodel 

SPMFRT=mpispmfrt-fftw2-omp
#SPMFRT=/usr/bin/spmfrt.ifc6
#SPMFRT=/usr/bin/spmfrt.ffc
#SPMFRT=/usr/bin/spmfrt.ssl2-ffc3
#MODS=*.d
MODS=*.mod

#FFLAGS=-H aesux

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out 

clean.all:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc *.log

.SUFFIXES: .f90 .out

.f90.out:
	$(SPMFRT) $(FFLAGS) $< -o $@
