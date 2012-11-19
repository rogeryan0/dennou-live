# Makefile for spmodel 

SPMFRT=/usr/bin/spmfrt
#SPMFRT=/usr/bin/spmfrt.ifc6
#SPMFRT=/usr/bin/spmfrt.ffc
#MODS=*.d
MODS=*.mod

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

.SUFFIXES: .f90 .out

.f90.out:
	$(SPMFRT) $< -o $@
