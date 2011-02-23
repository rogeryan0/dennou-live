# Makefile for spmodel 

SPMFRT=/usr/bin/spmfrt
#SPMFRT=/usr/bin/spmfrt.ifc6
#SPMFRT=/usr/bin/spmfrt.ffc
#MODS=*.d
MODS=*.mod

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out 

clean.all:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc *.gif *.xwd *.pnm *.log

.SUFFIXES: .f90 .out

.f90.out:
	$(SPMFRT) $< -o $@


kh1.nc: kh1.out
	./kh1.out

kh1-anim.gif: kh1.nc
	gtview kh1.nc@rho -shade -nocont srange=0.94,1.01 int=1e-3 -animate -smooth -aspect=3 -sw:ldump=.true. -sw:lwait1=.false.
	for i in dcl_*.xwd ; do convert $$i $${i/xwd/pnm} ; done
	rm dcl_*.xwd
	for i in dcl_*.pnm ; do pnmscale 0.5 $$i | ppmquant 256 | ppmtogif > $${i/pnm/gif} ; done
	rm dcl_*.pnm
	gifsicle --colors 256 dcl_*.gif dcl_051.gif dcl_051.gif dcl_051.gif dcl_051.gif dcl_051.gif > kh1-anim.gif
	mv dcl_051.gif kh1.gif
	rm dcl_*.gif

