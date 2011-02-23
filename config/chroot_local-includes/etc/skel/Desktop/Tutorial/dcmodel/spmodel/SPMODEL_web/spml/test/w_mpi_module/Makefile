# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt
MPIRUN=mpirun -np 2 
#MPIRUN=mpiexec -n 2 

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	w_base_mpi_test.out w_deriv_mpi_test.out w_integral_mpi_test.out \
	w_interpolate00_mpi_test.out w_interpolate01_mpi_test.out \
	w_interpolate10_mpi_test.out w_interpolate11_mpi_test.out \
	w_spectrum_mpi_test.out \
	w_spectrum_mpi_module_test.out
	$(MPIRUN) w_base_mpi_test.out
	$(MPIRUN) w_deriv_mpi_test.out
	$(MPIRUN) w_integral_mpi_test.out
	$(MPIRUN) w_interpolate00_mpi_test.out
	$(MPIRUN) w_interpolate01_mpi_test.out
	$(MPIRUN) w_interpolate10_mpi_test.out
	$(MPIRUN) w_interpolate11_mpi_test.out
	$(MPIRUN) w_spectrum_mpi_test.out
	$(MPIRUN) w_spectrum_mpi_module_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(MPIFC) -I../../src -I../../src-mpi $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

