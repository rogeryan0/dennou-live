#!/bin/sh
#
#= Script of checking GNU make
#
# Authors::   Yasuhiro Morikawa
# Version::   $Id: chkgmake.sh,v 1.1 2006/12/28 09:51:52 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2000-2006. All rights reserved.
# License::   See COPYRIGHT[link:COPYRIGHT]
#
#= Overview
#
# This script checks whether "make"s in PATH are GNU make.
#

report_me() {
	echo Error: $*
	echo please report it to GFD-Dennou Club.
	exit 1
}

help() {
	cat <<'END_OF_HELP'
chkgmake.sh - GNU make auto-detector

 --- input ---
    PATH       - $PATH environment variable
    MAKE       - specified GNU make command
 --- output ---
    output is written on 'chkgmake.cfg' (or \$OUT if set)
    MAKE       - GNU make
    
END_OF_HELP
	exit 1
}

#
# Step 1: initialization; read chkgmake.cfg if exists
#

output=${OUT:-chkgmake.cfg}

killcache=no
for arg in "$@"; do
	case $arg in
	-reinit|-nocache)
		killcache=yes
		;;
	-help)
		help
		;;
	-h)
		help
		;;
	esac
done
[ X"$killcache" = X"yes" ] && [ -f $output ] && rm -f $output

if [ -f $output ] && [ X"$FC" = X"" ]; then
	echo previous configuration result \`$output\' used.
	. ./$output
fi

#
# Step 2: search "make" or "gmake" in PATH;
#
make_list="$MAKE make gmake"
path_list=`echo $PATH | sed 's/:/ /g'`

for p in $path_list; do
    for m in make gmake ; do
	if [ -x "$p/$m" ]; then
	    make_list="$make_list $p/$m"
	fi
    done
done

echo "GNU make is searched in ... $make_list"

#
# Step 3: check echo options
#
case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

#
# Step 4: check whether "make"s in make_list are GNU make.
#

checked_gmake=
for m in $make_list ; do

    cat <<END_OF_TEST_PROGRAM > conftest.mk
%.xxx3: %.xxx1 %.xxx2
	cat $< $+ > \$@
END_OF_TEST_PROGRAM

    echo $ECHO_N "xxx1,$ECHO_C" > conftest.xxx1
    echo $ECHO_N "xxx2,$ECHO_C" > conftest.xxx2

    if $m -f conftest.mk conftest.xxx3 > /dev/null 2>&1 && [ -f conftest.xxx3 ] && [ 'xxx1,xxx1,xxx2,' = "`cat conftest.xxx3 | tr -d '\n'`" ] ;then
	checked_gmake=$m
	rm -f conftest.mk conftest.xxx1 conftest.xxx2 conftest.xxx3
	break
    else
	rm -f conftest.mk conftest.xxx1 conftest.xxx2 conftest.xxx3
	continue
    fi

done

#
# Step 5: set GNU make command to $MAKE environment variable
#

if [ -n "$checked_gmake" ]; then

    echo "GNU make is $checked_gmake"

    cat <<END_OF_REPORT > $output
 MAKE=$checked_gmake     ; export MAKE
END_OF_REPORT
    echo my guess about the GNU make is written onto $output.
    exit 0
else
    echo ""
    echo "Error: GNU make is not found in ..."
    echo "             $make_list"
    echo ""
    echo "       If you find GNU make in your system,"
    echo "       set the command to environment variable MAKE,"
    echo "       and execute again."
    echo ""
    exit 1
fi
