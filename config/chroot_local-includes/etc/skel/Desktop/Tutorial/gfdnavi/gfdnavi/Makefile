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
		"    cvs2cl -r -b -t --fsf -w --header <This header>\n" \
		"\n" \
		`cvs2cl --version` "\n" \
		"This script is available from "      \
		"http://www.red-bean.com/cvs2cl/\n\n" \
		    | cvs2cl -r -b -t --fsf -w --header -
	rm ChangeLog.bak
	@echo -e "\n" "ChangeLog is Updated." "\n"
