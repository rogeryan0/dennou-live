# Makefile for spmodel library document
#
# Version:: $Id: Makefile,v 1.22 2009-09-07 07:26:48 takepiro Exp $
#
# include ../Config.mk

LATEX=platex
DVIPDFM=dvipdfmx
MKDIR=mkdir
INSTALL=cp
CPP=cpp
HTMLDIR=../html

RDOC=/usr/bin/rdoc1.8
#RDOC=/var/lib/gems/1.8/bin/rdoc-f95
RDOCOPT=--ignore-case --charset euc-jp  \
	--inline-source --op html/ \
	--title "SPML References Manual" -U 
RDOCLIB=/GFD_Dennou_Club/ftp/arch/dcmodel/lib/ruby/1.8

F90FILES=src/*.f90 src-mpi/*.f90 doc/*.rdoc
MAINPAGE=doc/spml_modules.rdoc

PDF=chebyshev.pdf cheb_gal.pdf chebyshev_origin.pdf \
	w_module.pdf w_module_sjpack.pdf spectral_radial.pdf
HTML=INSTALL.html INSTALL.html.ja

all: $(PDF) html
html:
	make -f ./Makefile.rd2html

install: all
	cd ../src/ ; $(CPP) -P -D LAPACK eigmatrix.g90 > eigmatrix.f90
	cd .. ; RUBYLIB=$(RDOCLIB) $(RDOC) $(RDOCOPT) \
			--main $(MAINPAGE) $(F90FILES)
	rm ../src/eigmatrix.f90
	$(INSTALL) $(PDF) $(HTMLDIR)/
	$(INSTALL) $(HTML) $(HTMLDIR)/

clean:
	-rm -f *.pdf
	-rm -rf INSTALL.html INSTALL.html.ja

clean.all: clean
	-make -f Makefile.rd2html clean
	-rm -rf $(HTMLDIR)/

distclean: clean.all
.SUFFIXES: .tex .pdf
.tex.pdf:
	$(LATEX) $<
	$(LATEX) $<
	$(LATEX) $<
	$(LATEX) $<
	$(DVIPDFM) $*.dvi 
	rm -f *.bak *.dvi *.aux *.log *~
