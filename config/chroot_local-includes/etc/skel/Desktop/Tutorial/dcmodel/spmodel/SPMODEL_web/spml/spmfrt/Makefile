# Makefile for spmfrt

include ../Config.mk

TARGET=spm

ifeq (X$(MPIFC),X)
	ALLTARGET=$(TARGET)frt $(TARGET)config
else
	ALLTARGET=$(TARGET)frt $(TARGET)config mpi$(TARGET)frt
endif

all: $(ALLTARGET)

install: $(TARGET)frt
	test -d $(DEST_BIN) || $(INSTALLDIR) $(DEST_BIN)
	cp $(TARGET)frt $(DEST_BIN)
	cp $(TARGET)config $(DEST_BIN)
	if [ X$(MPIFC) != X ]; then \
		cp mpi$(TARGET)frt $(DEST_BIN) ;\
	fi

$(TARGET)frt: $(TARGET)frtgen.sh Makefile ../Config.mk
	@echo "Making shell script $(TARGET)frt"
	LIBNAME=$(LIBNAME) MODINSTDIR=$(DEST_INC) \
		LIBINSTDIR=$(DEST_LIB) \
		SYSLDFLAGS="$(SYSLDFLAGS)" \
		SYSLDLIBS="$(SYSLDLIBS)" \
		SYSFFLAGS="$(SYSFFLAGS)" \
		F90MODTYPE=$(F90MODTYPE) FC=$(FC) \
		sh ./$(TARGET)frtgen.sh $@
	chmod +x $(TARGET)frt

$(TARGET)config: $(TARGET)configgen.sh Makefile ../Config.mk
	@echo "Making shell script $(TARGET)config"
	LIBNAME=$(LIBNAME) MODINSTDIR=$(DEST_INC) \
		LIBINSTDIR=$(DEST_LIB) \
		SYSLDFLAGS="$(SYSLDFLAGS)" \
		SYSLDLIBS="$(SYSLDLIBS)" \
		SYSFFLAGS="$(SYSFFLAGS)" \
		F90MODTYPE=$(F90MODTYPE) FC=$(FC) \
		sh ./$(TARGET)configgen.sh $@
	chmod +x $(TARGET)config

mpi$(TARGET)frt: $(TARGET)frtgen.sh Makefile ../Config.mk
	@echo "Making shell script mpi$(TARGET)frt"
	LIBNAME=$(LIBNAME) MODINSTDIR=$(DEST_INC) \
		LIBINSTDIR=$(DEST_LIB) \
		SYSLDFLAGS="$(SYSLDFLAGS)" \
		SYSLDLIBS="$(SYSLDLIBS)" \
		SYSFFLAGS="$(SYSFFLAGS)" \
		F90MODTYPE=$(F90MODTYPE) FC=$(MPIFC) \
		sh ./$(TARGET)frtgen.sh $@
	chmod +x mpi$(TARGET)frt

clean:
	-rm -f *~ $(TARGET)frt $(TARGET)config mpi$(TARGET)frt
