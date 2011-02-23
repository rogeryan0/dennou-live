#!/bin/sh
#
# Copyright (C) GFD Dennou Club, 2002-2005.  All rights reserved.
#
fc="`which ${FC:?}`"
fflags="-I${MODINSTDIR:?} ${SYSFFLAGS} ${FFLAGS}"
libs="-L${LIBINSTDIR:?} -l${LIBNAME:?}"

ldflags="${SYSLDFLAGS} ${LDFLAGS}"
ldlibs="${SYSLDLIBS:?} ${LDLIBS}"
out=${1:-spmfrt}

if [ ${F90MODTYPE:?} = intel.d ]; then
	ldlibs=`echo $ldlibs | sed s/-lm//`
fi

cat > $out <<EOF
#!/bin/sh
#=begin
#== NAME
#((* $out *)) -- script for fortran usin spml
#
#Wrapper shell script for fortran compiler using spml
#
#== SYNTAX
#
# % $out [fortran_compiler_option] fortran_program.f90
#
#== DESCRIPTION
#
#((* $out *)) is simple shell script.
#This script set environment variables for using spml library.
#
#== OPTION
#
#:fortran_compiler_option
#  commonly used fortran compiler option, see your fortran compiler manual
#
#== BUGS
#
#If you find a bug, please report it at SPMODEL Development Group
#<dcstaff_(at)_gfd-dennou.org>
#=end

EOF


cat >> $out <<EOF
fc="$fc"
fflags="$fflags"
libs="$libs"
ldflags="$ldflags"
ldlibs="$ldlibs"

EOF

if [ ${F90MODTYPE:?} = intel.d ]; then

	cat >> $out <<EOF
# support for Intel Fortran
	if [ ! -f work.pcl ]; then
		echo work.pc > work.pcl
	fi
	for arg in \$fflags
	do
	    case "\$arg" in
	    -I*)
		dir=\`echo \$arg | sed s/-I//\`
		if [ ! -f \$dir/work.pc ]; then
			echo Notice: \$dir/work.pc not exist, and ignored.
			touch work.pcl
			break
		fi
		if ! grep -q "^\$dir/work.pc\\$" work.pcl
		then
			echo adding \$dir
			echo \$dir/work.pc >> work.pcl
		fi
		;;
	    esac
	done
EOF

fi

cat >> $out <<EOF

echo \$fc \$fflags "\$@" \$libs \$ldflags \$ldlibs
exec \$fc \$fflags "\$@" \$libs \$ldflags \$ldlibs
EOF
