# Makefile for spmpdel library

include ../Config.mk

TARGET=../src/lib$(LIBNAME).a

all: $(TARGET)

install: $(TARGET)
	test -d $(DEST_INC) || $(INSTALLDIR) $(DEST_INC)
	$(INSTALL) $(MODS) $(MODINCS) $(DEST_INC)
	if [ X$(F90MODTYPE) = X"intel.d" ]; then \
		$(INSTALL) work.pc $(DEST_INC)/work.pc ;\
	fi

OBJS = eee_mpi_module.o w_base_mpi_module.o w_deriv_mpi_module.o \
	w_integral_mpi_module.o w_mpi_module.o wa_base_mpi_module.o \
	wa_deriv_mpi_module.o wa_integral_mpi_module.o \
	wa_mpi_module.o wt_mpi_module.o 
#wt_galerkin_mpi_module.o

$(TARGET): $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

$(OBJS): work.pcl

work.pcl: Makefile ../Config.mk
	sh ../chkpcl.sh $(FFLAGS) $(SYSFFLAGS)

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.a 

#== Suffixes rules
#
# [JAPANESE] サフィックスルール
#
%.o: %.f90
	$(MPIFC) -c -I../src $(FFLAGS) $(SYSFFLAGS) $(INCLUDES) $<

# dependences

w_deriv_mpi_module.o: w_base_mpi_module.o
w_integral_mpi_module.o: w_base_mpi_module.o
w_mpi_module.o:	w_base_mpi_module.o w_deriv_mpi_module.o w_integral_mpi_module.o
wa_base_mpi_module.o: w_base_mpi_module.o
wa_deriv_mpi_module.o: wa_base_mpi_module.o
wa_integral_mpi_module.o: w_base_mpi_module.o
wa_mpi_module.o: wa_base_mpi_module.o wa_deriv_mpi_module.o wa_integral_mpi_module.o
wt_mpi_module.o: wa_mpi_module.o
#wt_galerkin_mpi_module.o:wt_mpi_module.o

