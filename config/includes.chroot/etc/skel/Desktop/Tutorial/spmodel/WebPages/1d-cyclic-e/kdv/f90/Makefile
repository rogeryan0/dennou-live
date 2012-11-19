# Makefile for spmodel 

SPMFRT=/usr/bin/spmfrt
#SPMFRT=/usr/bin/spmfrt.ifc6
#SPMFRT=/usr/bin/spmfrt.ffc
#MODS=*.d
MODS=*.mod

clean:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.gif *.pbm *.xwd

clean.all:
	-rm -f $(MODS) *.o work.pc* *~ *.bak *.out *.nc *.gif *.pbm *.xwd

.SUFFIXES: .f90 .out

.f90.out:
	$(SPMFRT) $< -o $@

kdv1.nc: kdv1.out
	./kdv1.out

kdv1-anim.gif: kdv1.nc
	gtview slice=t=^1 kdv1.nc@zeta -animate -smooth -sw:ldump=.true. -sw:lwait1=.false.
	for i in dcl_*.xwd ; do convert $$i $${i/xwd/pbm} ; done
	rm dcl_*.xwd
	for i in dcl_*.pbm ; do pnmscale 0.5 $$i | ppmquant 256 | ppmtogif > $${i/pbm/gif} ; done
	rm dcl_*.pbm
	gifsicle --colors 256 dcl_001.gif dcl_001.gif dcl_001.gif dcl_*.gif dcl_051.gif dcl_051.gif dcl_051.gif dcl_051.gif > kdv1-anim.gif
	mv dcl_051.gif kdv1.gif
	rm dcl_*.gif

install:: kdv1-anim.gif
	cp kdv1.gif kdv1-anim.gif 
	   /GFD_Dennou_Club/ftp/arch/spmodel/gallery/kdv/

