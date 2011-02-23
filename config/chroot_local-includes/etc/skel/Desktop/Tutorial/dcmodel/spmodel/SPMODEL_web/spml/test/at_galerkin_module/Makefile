# Makefile for spmodel library
#
# $Id: Makefile,v 1.3 2009-01-16 16:23:04 uwabami Exp $
# 

include ../../Config.mk

SPMFRT=$(DEST_BIN)/spmfrt

all: clean

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc

test:	at_ap_galerkin_DN_test1d.out at_ap_galerkin_DN_test2d.out \
	at_aq_galerkin_RR_test1d.out at_ab_galerkin_ND_test1d.out \
	at_aq_galerkin_RR_test2d.out at_ab_galerkin_ND_test2d.out \
	at_aq_galerkin_RS_test1d.out at_ad_galerkin_DD_test1d.out \
	at_aq_galerkin_SR_test1d.out at_ad_galerkin_DD_test2d.out \
	at_aq_galerkin_SS_test1d.out at_af_galerkin_MM_test1d.out \
	at_aq_galerkin_SS_test2d.out at_af_galerkin_MM_test2d.out \
	at_av_galerkin_NN_test1d.out at_ah_galerkin_MMex_test1.out \
	at_av_galerkin_NN_test2d.out
	./at_ap_galerkin_DN_test1d.out
	./at_ap_galerkin_DN_test2d.out
	./at_aq_galerkin_RR_test1d.out
	./at_ab_galerkin_ND_test1d.out
	./at_aq_galerkin_RR_test2d.out
	./at_ab_galerkin_ND_test2d.out
	./at_aq_galerkin_RS_test1d.out
	./at_ad_galerkin_DD_test1d.out
	./at_aq_galerkin_SR_test1d.out
	./at_ad_galerkin_DD_test2d.out
	./at_aq_galerkin_SS_test1d.out
	./at_af_galerkin_MM_test1d.out
	./at_aq_galerkin_SS_test2d.out
	./at_af_galerkin_MM_test2d.out
	./at_av_galerkin_NN_test1d.out
	./at_ah_galerkin_MMex_test1.out
	./at_av_galerkin_NN_test2d.out

.SUFFIXES: .f90 .out

.f90.out:
	$(FC) -I../../src  $(SYSFFLAGS) -o $@ $< -L../../src -l$(LIBNAME) $(SYSLDFLAGS) $(SYSLDLIBS)
