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


#gravcurrent1.nc: gravcurrent1.out
#	./gravcurrent1.out

gravcurrent1-anim.gif: gravcurrent1.nc
	cp gravcurrent1.nc tmp.nc
	gtattr tmp.nc@x -delete=topology
	gtview tmp.nc@rho,x=0:60 -shade -nocont srange=0.9998,1.0012 int=2e-5 -animate -smooth -aspect=3 -sw:ldump=.true. -sw:lwait1=.false.
	rm tmp.nc
	for i in dcl_*.xwd ; do convert $$i $${i/xwd/pnm} ; done
	rm dcl_*.xwd
	for i in dcl_*.pnm ; do pnmscale 0.5 $$i | ppmquant 256 | ppmtogif > $${i/pnm/gif} ; done
	rm dcl_*.pnm
	gifsicle --delay 100 --colors 256 dcl_001.gif dcl_001.gif dcl_001.gif dcl_001.gif dcl_001.gif dcl_*.gif dcl_031.gif dcl_031.gif dcl_031.gif dcl_031.gif dcl_031.gif > gravcurrent1-anim.gif
	mv dcl_020.gif gravcurrent1.gif
	rm dcl_*.gif
