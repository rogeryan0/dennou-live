# Makefile for spmpdel library

include ../Config.mk

TARGET=lib$(LIBNAME).a

EIGENOBJS=
MATRIXOBJS=

ifeq ($(MATRIXLIB),SSL2)
	EIGENOBJS=ssl2_eigen.o 
	MATRIXOBJS=eigmatrix.o $(EIGENOBJS)
endif
ifeq ($(MATRIXLIB),SSL2TP)
	EIGENOBJS=ssl2tp_eigen.o 
	MATRIXOBJS=eigmatrix.o $(EIGENOBJS)
endif
ifeq ($(MATRIXLIB),LAPACK)
	EIGENOBJS=lapack_eigen.o 
	MATRIXOBJS=eigmatrix.o $(EIGENOBJS)
endif


all: $(TARGET)

install: $(TARGET)
	test -d $(DEST_LIB) || $(INSTALLDIR) $(DEST_LIB)
	test -d $(DEST_INC) || $(INSTALLDIR) $(DEST_INC)
	$(INSTALL) $(TARGET) $(DEST_LIB)
	$(INSTALL) $(MODS) $(MODINCS) $(DEST_INC)
	if [ X$(F90MODTYPE) = X"intel.d" ]; then \
		$(INSTALL) work.pc $(DEST_INC)/work.pc ;\
	fi


OBJS =	lumatrix_f77.o lumatrix.o \
	ae_module.o at_module.o au_module.o aq_module.o \
	ee_module.o esc_module.o c2ajcc.o ef_module.o \
	et_module.o eq_module.o eee_module.o tee_module.o \
	ae_module_fftj.o ee_module_fftj.o et_module_fftj.o \
	w_base_module.o w_integral_module.o \
	w_deriv_module.o w_interpolate_module.o gamma.o dlgamma.o \
	w_spectrum_module.o  w_module.o \
	wa_base_module.o wa_integral_module.o \
	wa_deriv_module.o wa_interpolate_module.o \
	wa_spectrum_module.o wa_module.o wq_module.o \
	wt_module.o wu_module.o wtu_module.o \
	w_base_module_sjpack.o w_integral_module_sjpack.o \
	w_deriv_module_sjpack.o w_interpolate_module_sjpack.o \
	w_spectrum_module_sjpack.o  w_module_sjpack.o \
	wa_base_module_sjpack.o wa_deriv_module_sjpack.o \
	wa_integral_module_sjpack.o wa_spectrum_module_sjpack.o \
	wa_interpolate_module_sjpack.o wa_module_sjpack.o \
	wt_module_sjpack.o wq_module_sjpack.o \
	at_ab_galerkin_ND.o at_ad_galerkin_DD.o at_af_galerkin_MM.o     \
	at_ah_galerkin_MMex.o at_ap_galerkin_DN.o at_aq_galerkin_RRFF.o \
	at_av_galerkin_NN.o et_galerkin_module.o et_galerkin_module_fftj.o \
	l_module.o la_module.o \
	w_zonal_module.o wa_zonal_module.o wt_zonal_module.o \
	l_module_sjpack.o la_module_sjpack.o \
	w_zonal_module_sjpack.o wa_zonal_module_sjpack.o \
	wt_zonal_module_sjpack.o \
	wt_galerkin_module.o wt_galerkin_module_sjpack.o \
	wt_zonal_galerkin_module.o wt_zonal_galerkin_module_sjpack.o \
	indexx.o dsort_netlib.o \
	$(MATRIXOBJS)

$(TARGET): $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

$(OBJS): work.pcl

work.pcl: Makefile ../Config.mk
	sh ../chkpcl.sh $(FFLAGS) $(SYSFFLAGS)

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.a eigmatrix.f90

# dependences

at_module.o: lumatrix.o
aq_module.o: lumatrix.o gamma.o

et_module.o: ae_module.o at_module.o lumatrix.o
eq_module.o: ae_module.o aq_module.o lumatrix.o

w_integral_module.o: w_base_module.o
w_deriv_module.o: w_base_module.o
w_spectrum_module.o: w_base_module.o
w_interpolate_module.o: gamma.o w_base_module.o
w_module.o: w_spectrum_module.o w_integral_module.o \
	    w_deriv_module.o w_interpolate_module.o w_base_module.o 

wa_spectrum_module.o: w_base_module.o
wa_interpolate_module.o: gamma.o w_base_module.o
wa_base_module.o: w_base_module.o
wa_integral_module.o: w_base_module.o
wa_deriv_module.o: wa_base_module.o w_base_module.o w_deriv_module.o
wa_module.o: wa_base_module.o w_base_module.o \
	     wa_integral_module.o w_integral_module.o \
	     wa_interpolate_module.o w_interpolate_module.o \
	     wa_deriv_module.o w_deriv_module.o \
	     wa_spectrum_module.o w_spectrum_module.o

wt_module.o: wa_module.o at_module.o lumatrix.o
wu_module.o: wa_module.o au_module.o lumatrix.o
wtu_module.o: wt_module.o wu_module.o
wq_module.o: wa_module.o aq_module.o lumatrix.o

w_integral_module_sjpack.o: w_base_module_sjpack.o
w_deriv_module_sjpack.o: w_base_module_sjpack.o
w_spectrum_module_sjpack.o: w_base_module_sjpack.o
w_interpolate_module_sjpack.o: gamma.o w_base_module_sjpack.o
w_module_sjpack.o: w_spectrum_module_sjpack.o w_integral_module_sjpack.o \
	w_deriv_module_sjpack.o w_interpolate_module_sjpack.o w_base_module_sjpack.o

wa_spectrum_module_sjpack.o: w_base_module_sjpack.o
wa_interpolate_module_sjpak.o: gamma.o w_basae_module_sjpack.o
wa_base_module_sjpack.o: w_base_module_sjpack.o
wa_integral_module_sjpack.o: w_base_module_sjpack.o
wa_deriv_module_sjpack.o: wa_base_module_sjpack.o w_base_module_sjpack.o w_deriv_module_sjpack.o
wa_module_sjpack.o: wa_base_module_sjpack.o w_base_module_sjpack.o \
	wa_integral_module_sjpack.o w_integral_module_sjpack.o \
	wa_interpolate_module_sjpack.o w_interpolate_module_sjpack.o \
	wa_deriv_module_sjpack.o w_deriv_module_sjpack.o \
	wa_spectrum_module_sjpack.o w_spectrum_module_sjpack.o

wt_module_sjpack.o: wa_module_sjpack.o at_module.o lumatrix.o
wq_module_sjpack.o: wa_module_sjpack.o aq_module.o lumatrix.o

at_ab_galerkin_ND.o:at_module.o
at_ad_galerkin_DD.o:at_module.o
at_af_galerkin_MM.o:at_module.o
at_ah_galerkin_MMex.o:at_module.o
at_ap_galerkin_DN.o:at_module.o
at_aq_galerkin_RRFF.o:at_module.o
at_av_galerkin_NN.o:at_module.o
et_galerkin_module.o:et_module.o \
	at_ab_galerkin_ND.o at_ad_galerkin_DD.o at_af_galerkin_MM.o  \
	at_ah_galerkin_MMex.o at_ap_galerkin_DN.o at_aq_galerkin_RRFF.o \
	at_av_galerkin_NN.o
et_galerkin_module_fftj.o:et_module_fftj.o \
	at_ab_galerkin_ND.o at_ad_galerkin_DD.o at_af_galerkin_MM.o  \
	at_ah_galerkin_MMex.o at_ap_galerkin_DN.o at_aq_galerkin_RRFF.o \
	at_av_galerkin_NN.o
wt_galerkin_module.o:wt_module.o \
	at_ab_galerkin_ND.o at_ad_galerkin_DD.o at_af_galerkin_MM.o \
	at_ah_galerkin_MMex.o at_ap_galerkin_DN.o at_aq_galerkin_RRFF.o \
	at_av_galerkin_NN.o
wt_galerkin_module_sjpack.o:wt_module_sjpack.o \
	at_ab_galerkin_ND.o at_ad_galerkin_DD.o at_af_galerkin_MM.o \
	at_ah_galerkin_MMex.o at_ap_galerkin_DN.o at_aq_galerkin_RRFF.o \
	at_av_galerkin_NN.o
wt_zonal_module.o:wa_zonal_module.o at_module.o
wt_zonal_module_sjpack.o:wa_zonal_module_sjpack.o at_module.o
wt_zonal_galerkin_module.o:wt_zonal_module.o \
	at_ab_galerkin_ND.o at_ad_galerkin_DD.o at_af_galerkin_MM.o \
	at_ah_galerkin_MMex.o at_ap_galerkin_DN.o at_aq_galerkin_RRFF.o \
	at_av_galerkin_NN.o
wt_zonal_galerkin_module_sjpack.o:wt_zonal_module_sjpack.o \
	at_ab_galerkin_ND.o at_ad_galerkin_DD.o at_af_galerkin_MM.o \
	at_ah_galerkin_MMex.o at_ap_galerkin_DN.o at_aq_galerkin_RRFF.o \
	at_av_galerkin_NN.o
wa_zonal_module.o:w_zonal_module.o la_module.o
w_zonal_module.o: l_module.o
la_module.o: l_module.o

eigmatrix.f90: eigmatrix.g90
	$(CPP) -P -D $(MATRIXLIB) $< > $@

eigmatrix.o: $(EIGENOBJS)

ssl2_eigen.o: indexx.o
ssl2tp_eigen.o: indexx.o
lapack_eigen.o: indexx.o
