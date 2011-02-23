# Makefile for spmodel library

include ../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

.SUFFIXES: .f90 .out

.f90.out:
	$(SPMFRT) $< -o $@
