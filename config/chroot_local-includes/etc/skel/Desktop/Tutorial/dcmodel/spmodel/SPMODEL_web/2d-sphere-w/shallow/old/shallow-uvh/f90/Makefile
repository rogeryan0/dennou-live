#----------------------------------------------------------------------
#     Copyright (c) 2003 Masatsugu Odaka. All rights reserved.
#----------------------------------------------------------------------
# Makefile for sample program for gtool_history/gtool4 and ISPACK  
# 2003/07/01 M. Odaka
#
SPMFC    = /usr/bin/spmfrt
SHELL	 = /bin/sh
RM	 = /bin/rm -f

TARGET   = shallow_uvh
TARGET_INIT = shallow_uvh_init
SRC      = $(TARGET).f90
SRC_INIT = $(TARGET_INIT).f90
NMFILE   = indata.nm
INITFILE = $(TARGET).nc

.SUFFIXES:.f90


.f90.o: 
	$(SPMFC) -c $< 


all: $(TARGET).o 
	$(SPMFC) -o $(TARGET) $(TARGET).o


init: $(TARGET_INIT)
	$(RM) $(TARGET_INIT).nc 
	./$(TARGET_INIT) < $(NMFILE)


$(TARGET_INIT): $(TARGET_INIT).o
	$(SPMFC) -o $(TARGET_INIT) $(TARGET_INIT).o


clean.all: clean.init clean


clean:
	$(RM) $(TARGET) $(TARGET).o *~ *.log 


clean.init:
	$(RM) $(TARGET_INIT) $(TARGET_INIT).o $(TARGET_INIT).nc 


