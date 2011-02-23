# Makefile for spmodel library

include ../Config.mk

MISCDIRS=at_galerkin_module misc

NMPIDIRS=eigmatrix lumatrix ae_module at_module au_module aq_module \
	ee_module esc_module et_module eq_module ef_module \
	eee_module tee_module \
	w_module wa_module wt_module wu_module wtu_module wq_module \
	l_module la_module w_zonal_module wa_zonal_module wt_zonal_module \
	ae_module_fftj ee_module_fftj et_module_fftj \
	w_module_sjpack wa_module_sjpack wt_module_sjpack wq_module_sjpack \
	l_module_sjpack la_module_sjpack \
	w_zonal_module_sjpack wa_zonal_module_sjpack # wt_zonal_module_sjpack

MPIDIRS=eee_mpi_module w_mpi_module wa_mpi_module wt_mpi_module

ifeq (X$(MPIFC),X)
	TESTDIRS=$(NMPIDIRS)
	SUBDIRS=$(NMPIDIRS) $(MISCDIRS)
else
	TESTDIRS=$(NMPIDIRS) $(MPIDIRS)
	SUBDIRS=$(NMPIDIRS) $(MPIDIRS) $(MISCDIRS)
endif

all: clean

test:
	for i in $(TESTDIRS) ; do \
	  cd $$i ; $(MAKE) test ;  \
	  cd ../ ; \
	done

clean: clean.local clean.subdirs

clean.subdirs:
	@for i in $(SUBDIRS) ; do \
	  cd $$i ; $(MAKE) clean ; \
	  cd ../ ; \
	done

clean.local:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc
