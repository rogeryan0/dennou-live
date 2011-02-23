#!/bin/sh

# update Intel Fortran's work.pcl file

if [ ! -f work.pcl ]; then
	echo work.pc > work.pcl
fi

for arg in "$@"
do
	case "$arg" in
	-I*)
		dir=`echo $arg | sed s/-I//`
		if [ ! -f $dir/work.pc ]; then
			echo Notice: $dir/work.pc not exist, and ignored.
			touch work.pcl
			break
		fi
		if ! grep -q "^$dir/work.pc\$" work.pcl
		then
			echo adding $dir
			echo $dir/work.pc >> work.pcl
		fi
		;;
	esac
done
