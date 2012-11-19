# Makefile for spmodel 

SPMFRT=spmfrt
#SPMFRT=/usr/bin/spmfrt.ffc
#SPMFRT=/usr/bin/spmfrt.ifc6
#MODS=*.mod
MODS=*.d

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

.SUFFIXES: .f90 .out

.f90.out:
	$(SPMFRT) $< -o $@
