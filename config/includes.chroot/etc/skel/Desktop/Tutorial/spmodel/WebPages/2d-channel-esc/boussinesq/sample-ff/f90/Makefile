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

bsncnv-ff-1.nc: bsncnv-ff-1.out
	./bsncnv-ff-1.out

bsncnv-ff-1-anim.gif: bsncnv-ff-1.nc
	gtview bsncnv-ff-1.nc@temp -shade srange=0,1 crange=0,1 int=0.1 aspect=8 -animate -smooth -sw:ldump=.true. -sw:lwait1=.false.
	for i in dcl_*.xwd ; do convert $$i $${i/xwd/pnm} ; done
	rm dcl_*.xwd
	for i in dcl_*.pnm ; do pnmscale 0.5 $$i | ppmquant 256 | ppmtogif > $${i/pnm/gif} ; done
	rm dcl_*.pnm
	gifsicle --delay 100 --colors 256 dcl_001.gif dcl_001.gif dcl_001.gif dcl_*.gif dcl_021.gif dcl_021.gif dcl_021.gif dcl_021.gif > bsncnv-ff-1-anim.gif
	mv dcl_021.gif bsncnv-ff-1.gif
	rm dcl_*.gif

install:: bsncnv-ff-1-anim.gif 
	cp bsncnv-ff-1.gif bsncnv-ff-1-anim.gif \
	   /GFD_Dennou_Club/ftp/arch/spmodel/gallery/bsncnv/

