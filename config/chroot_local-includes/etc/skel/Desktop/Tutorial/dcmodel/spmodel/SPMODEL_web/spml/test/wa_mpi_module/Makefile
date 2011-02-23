# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt
MPIRUN=mpirun -n 1
#MPIRUN=mpiexec -n 2 

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	wa_base_mpi_test.out wa_deriv_mpi_test.out wa_integral_mpi_test.out \
	wa_interpolate00_mpi_test.out wa_interpolate01_mpi_test.out \
	wa_interpolate10_mpi_test.out wa_interpolate11_mpi_test.out \
	wa_spectrum_mpi_test.out wa_spectrum_mpi_module_test.out
	$(MPIRUN) wa_base_mpi_test.out
	$(MPIRUN) wa_deriv_mpi_test.out
	$(MPIRUN) wa_integral_mpi_test.out
	$(MPIRUN) wa_interpolate00_mpi_test.out
	$(MPIRUN) wa_interpolate01_mpi_test.out
	$(MPIRUN) wa_interpolate10_mpi_test.out
	$(MPIRUN) wa_interpolate11_mpi_test.out
	$(MPIRUN) wa_spectrum_mpi_test.out
	$(MPIRUN) wa_spectrum_mpi_module_test.out

.SUFFIXES: .f90 .out

.f90.out:
	$(MPIFC) -I../../src -I../../src-mpi $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

