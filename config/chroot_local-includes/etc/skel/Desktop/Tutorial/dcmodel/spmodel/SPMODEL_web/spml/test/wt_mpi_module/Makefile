# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt
MPIRUN=mpirun -n 2 
#MPIRUN=mpiexec -n 2 

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	wt_mpi_base_test.out wt_mpi_derivative_test1.out \
	wt_mpi_derivative_test2.out wt_mpi_derivative_test3.out \
	wt_mpi_derivative_test4.out wt_mpi_derivative_test5.out \
	wt_mpi_derivative_test6.out wt_mpi_derivative_test7.out \
	wt_mpi_intavr_test.out
	$(MPIRUN) wt_mpi_base_test.out
	$(MPIRUN) wt_mpi_derivative_test1.out
	$(MPIRUN) wt_mpi_derivative_test2.out
	$(MPIRUN) wt_mpi_derivative_test3.out
	$(MPIRUN) wt_mpi_derivative_test4.out
	$(MPIRUN) wt_mpi_derivative_test5.out
	$(MPIRUN) wt_mpi_derivative_test6.out
	$(MPIRUN) wt_mpi_derivative_test7.out
	$(MPIRUN) wt_mpi_intavr_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(MPIFC) -I../../src -I../../src-mpi $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

