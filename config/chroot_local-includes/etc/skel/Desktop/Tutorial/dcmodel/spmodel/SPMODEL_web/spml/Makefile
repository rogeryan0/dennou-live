# Makefile for spmodel library

include Config.mk

all:
	cd src; $(MAKE)
	cd spmfrt; $(MAKE) $@
	if [ X$(MPIFC) != X ]; then \
		cd src-mpi; $(MAKE) ;\
	fi

install: all
	cd src; $(MAKE) $@
	cd spmfrt; $(MAKE) $@
	if [ X$(MPIFC) != X ]; then \
		cd src-mpi; $(MAKE) $@ ;\
	fi

html:
	cd doc; $(MAKE)
	cd doc; $(MAKE) install

test: check

check:
	cd test ; $(MAKE) test

clean: clean.local
	cd src; $(MAKE) $@
	cd src-mpi; $(MAKE) $@
	cd spmfrt; $(MAKE) $@
	cd demo; $(MAKE) $@
	cd test; $(MAKE) $@
	cd doc; $(MAKE) $@
	cd supplemental; $(MAKE) $@

clean.local:
	-rm *.bak *~

distclean: clean
	-rm -r html
	-rm -f config.status chkfort.cfg chkgmake.cfg
	-rm -f config.cache config.log Config.mk
	@echo ""
	@echo "    Removing Config.mk,"
	@echo "      you should rerun ./configure to build spmodel library."
	@echo ""

cl:
	@cvs2cl --help > /dev/null 2>&1 || \
	    echo -e \
		"\n" \
		"    cvs2cl is not found.\n" \
		"    Please get from http://www.red-bean.com/cvs2cl/\n"
	@cvs2cl --help > /dev/null 2>&1 && \
	    echo -e \
		"This file is generated automatically by\n" \
		"\n" \
		"    cvs2cl.pl -r -b -t --fsf -w --header <This header>\n" \
		"\n" \
		`cvs2cl --version` "\n" \
		"This script is available from "      \
		"http://www.red-bean.com/cvs2cl/\n\n" \
		    | cvs2cl -r -b -t --fsf -w --header -
	rm ChangeLog.bak
	@echo -e "\n" "ChangeLog is Updated." "\n"

