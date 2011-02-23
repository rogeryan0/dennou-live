# Makefile for index.html in /GFD_Dennou_Club/ftp/arch/ruby/products/ep_flux/tutorial/
#
# 2004/10/26 Tsukahara Daisuke

include ./Mkinclude
# Redefine
CSS     = ./dennou-ruby.css

RDFILES       = index.html
INDEXFILES    = index.hindex

RD2HTML = ${RD2} -r ${RDEXT} ${RDFLAG} ${CSSFLAG}

all: rd2html chmod chgrp

rd2html: $(INDEXFILES) $(RDFILES) hindex-clean

chmod:
	$(CHMOD) $(MODE) $(RDFILES)

chgrp:
	$(CHGRP) $(GID) $(RDFILES)

clean: hindex-clean
	rm -rf $(RDFILES) *~

hindex-clean:
	rm -rf ./*.hindex.rd

# sync: 
#	rsync -urlptv --delete * dennou-k.gfd-dennou.org:/GFD_Dennou_Club/dc-arch/cc-env/cvs/

.SUFFIXES : .html .rd
.rd.html:
	$(RD2HTML) $< > $@

.SUFFIXES : .hindex .rd
.rd.hindex:
	$(RUBY) $(RDHINDEX) $< > $@.rd
