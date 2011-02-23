# Makefile for spmodel library

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt
MPIRUN=mpirun -n 2 
#MPIRUN=mpiexec -n 2 

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	eee_mpi_test_gatherZeta.out eee_mpi_test_RotVelxVor.out \
	eee_mpi_test_gathering.out eee_mpi_test_ZetaVorVel.out \
	eee_mpi_test_intavr.out eee_mpi_test_derivative.out \
	eee_mpi_test_transform.out eee_mpi_test_energyhelicity.out
	$(MPIRUN) ./eee_mpi_test_gatherZeta.out	 
	$(MPIRUN) ./eee_mpi_test_RotVelxVor.out	 
	$(MPIRUN) ./eee_mpi_test_gathering.out	 
	$(MPIRUN) ./eee_mpi_test_ZetaVorVel.out	 
	$(MPIRUN) ./eee_mpi_test_intavr.out	 
	$(MPIRUN) ./eee_mpi_test_derivative.out	 
	$(MPIRUN) ./eee_mpi_test_transform.out	 
	$(MPIRUN) ./eee_mpi_test_energyhelicity.out

.SUFFIXES: .f90 .out

.f90.out:
	$(MPIFC) -I../../src -I../../src-mpi $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)

