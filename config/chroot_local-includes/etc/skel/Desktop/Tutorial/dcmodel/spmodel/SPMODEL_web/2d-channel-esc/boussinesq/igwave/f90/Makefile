# Makefile for spmodel 

SPMFRT=/usr/bin/spmfrt
#SPMFRT=/usr/bin/spmfrt.ifc6
#SPMFRT=/usr/bin/spmfrt.ffc
#MODS=*.d
MODS=*.mod

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.gif *.pnm *.xwd

clean.all:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc *.gif *.pnm *.xwd

.SUFFIXES: .f90 .out

.f90.out:
	$(SPMFRT) $< -o $@

igwave1-omega0.3.nc: igwave1.out
	echo 0.3 | ./igwave1.out
	mv igwave1.nc igwave1-omega0.3.nc

igwave1-omega0.6.nc: igwave1.out
	echo 0.6 | ./igwave1.out
	mv igwave1.nc igwave1-omega0.6.nc

igwave1-omega0.9.nc: igwave1.out
	echo 0.9 | ./igwave1.out
	mv igwave1.nc igwave1-omega0.9.nc

igwave1-omega1.2.nc: igwave1.out
	echo 1.2 | ./igwave1.out
	mv igwave1.nc igwave1-omega1.2.nc

igwave1-omega0.3-anim.gif:igwave1-omega0.3.nc
	gtview igwave1-omega0.3.nc@rho -animate -shade -smooth int=2e-6 srange=-4e-5,4e-5 -nocont -aspect=2 -sw:ldump=.true. -sw:lwait1=.false.
	for i in dcl_*.xwd ; do convert $$i $${i/xwd/pnm} ; done
	rm dcl_*.xwd
	for i in dcl_*.pnm ; do pnmscale 0.5 $$i | ppmquant 256 | ppmtogif > $${i/pnm/gif} ; done
	rm dcl_*.pnm
	gifsicle --delay 20 --colors 256 dcl_001.gif dcl_001.gif dcl_001.gif dcl_*.gif dcl_081.gif dcl_081.gif dcl_081.gif dcl_081.gif > igwave1-omega0.3-anim.gif
	mv dcl_081.gif igwave1-omega0.3.gif
	rm dcl_*.gif

igwave1-omega0.6-anim.gif:igwave1-omega0.6.nc
	gtview igwave1-omega0.6.nc@rho -animate -shade -smooth int=5e-6 srange=-1e-4,1e-4 -nocont -aspect=2 -sw:ldump=.true. -sw:lwait1=.false.
	for i in dcl_*.xwd ; do convert $$i $${i/xwd/pnm} ; done
	rm dcl_*.xwd
	for i in dcl_*.pnm ; do pnmscale 0.5 $$i | ppmquant 256 | ppmtogif > $${i/pnm/gif} ; done
	rm dcl_*.pnm
	gifsicle --delay=20 --colors 256 dcl_001.gif dcl_001.gif dcl_001.gif dcl_*.gif dcl_081.gif dcl_081.gif dcl_081.gif dcl_081.gif > igwave1-omega0.6-anim.gif
	mv dcl_081.gif igwave1-omega0.6.gif
	rm dcl_*.gif

igwave1-omega0.9-anim.gif:igwave1-omega0.9.nc
	gtview igwave1-omega0.9.nc@rho -animate -shade -smooth int=1e-5 srange=-2e-4,2e-4 -nocont -aspect=2 -sw:ldump=.true. -sw:lwait1=.false.
	for i in dcl_*.xwd ; do convert $$i $${i/xwd/pnm} ; done
	rm dcl_*.xwd
	for i in dcl_*.pnm ; do pnmscale 0.5 $$i | ppmquant 256 | ppmtogif > $${i/pnm/gif} ; done
	rm dcl_*.pnm
	gifsicle --delay 20 --colors 256 dcl_001.gif dcl_001.gif dcl_001.gif dcl_*.gif dcl_081.gif dcl_081.gif dcl_081.gif dcl_081.gif > igwave1-omega0.9-anim.gif
	mv dcl_081.gif igwave1-omega0.9.gif
	rm dcl_*.gif

igwave1-omega1.2-anim.gif:igwave1-omega1.2.nc
	gtview igwave1-omega1.2.nc@rho -animate -shade -smooth int=4e-6 srange=-8e-5,8e-5 -nocont -aspect=2 -sw:ldump=.true. -sw:lwait1=.false.
	for i in dcl_*.xwd ; do convert $$i $${i/xwd/pnm} ; done
	rm dcl_*.xwd
	for i in dcl_*.pnm ; do pnmscale 0.5 $$i | ppmquant 256 | ppmtogif > $${i/pnm/gif} ; done
	rm dcl_*.pnm
	gifsicle --delay 20 --colors 256 dcl_001.gif dcl_001.gif dcl_001.gif dcl_*.gif dcl_081.gif dcl_081.gif dcl_081.gif dcl_081.gif > igwave1-omega1.2-anim.gif
	mv dcl_081.gif igwave1-omega1.2.gif
	rm dcl_*.gif

install:: igwave1-omega0.3-anim.gif igwave1-omega0.6-anim.gif igwave1-omega0.9-anim.gif igwave1-omega1.2-anim.gif
	cp igwave1-omega0.3-anim.gif igwave1-omega0.3.gif \
	   igwave1-omega0.6-anim.gif igwave1-omega0.6.gif \
	   igwave1-omega0.9-anim.gif igwave1-omega0.9.gif \
	   igwave1-omega1.2-anim.gif igwave1-omega1.2.gif \
	   ../../../../gallery/igwave/

