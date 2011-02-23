#!/bin/sh
#
# Copyright (C) GFD Dennou Club, 2002-2005.  All rights reserved.
#
fc="`which ${FC:?}`"
fflags="-I${MODINSTDIR:?} $SYSFFLAGS $FFLAGS"
ldflags="$SYSLDFLAGS -L$LIBINSTDIR $LDFLAGS"
ldlibs="-l$LIBNAME $SYSLDLIBS $LDLIBS"
out=${1:-spmconfig}

if [ $F90MODTYPE = intel.d ]; then
	ldlibs=`echo $ldlibs | sed s/-lm//`
fi
cat > $out <<END_OF_SCRIPT
#!/bin/sh
#=begin
#== NAME
#((* $out *)) -- output FFLAGS, LDFLAGS, LDLIBS for using spml
#
#get environment variables using spml
#
#== SYNTAX
#
# % $out  [--fc] or [--fflags] or [--ldflags] or [--ldlibs]
#
#== DESCRIPTION
#
#((* $out *)) is simple shell script.
#This script output environment variables for using spml library.
#
#== OPTION
#
#:--fc
#  output fortran compiler name using spml libarary
#:--fflags
#  output fortran FFLAGS using spml libarary
#:--ldflags
#  output fortran LDFLAGS using spml libarary
#:--ldlibs
#  output fortran LDLIBS using spml libarary
#
#== BUGS
#
#If you find a bug, please report it at SPMODEL Development Group
#<dcstaff_(at)_gfd-dennou.org>
#
#=end

END_OF_SCRIPT

cat >> $out <<END_OF_SCRIPT
fc="$fc"
fflags="$fflags"
ldflags="$ldflags"
ldlibs="$ldlibs"

usage() {
	cat <<EOF
usage: $out [OPTIONS]
options: one of --fc --fflags --ldflags --ldlibs
EOF
	exit 1
}

if test \$# -eq 0; then
	usage
fi

while test \$# -gt 0; do
    case \$1 in
	--fc)		echo \$fc ;;
	--fflags)	echo \$fflags ;;
	--ldflags)	echo \$ldflags ;;
	--ldlibs)	echo \$ldlibs ;;
	*)		usage ;;
    esac
    shift
done

END_OF_SCRIPT
