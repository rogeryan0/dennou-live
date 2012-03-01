#!/bin/sh
# A script to creat Mkinclude file for the tutorial at davis workshop 2012.

export FC=g95
./configure \
   --with-netcdf=/usr/lib/libnetcdff-g95.a \
   --with-gtool5=/usr/lib/gtool5-g95/lib/libgtool5.a \
   --with-lapack=/usr/lib/liblapack-g95.a \
   --with-blas=/usr/lib/libblas-g95.a

cp Mkinclude Mkinclude.bk
sed -e "s/-lnetcdff-g95/-lnetcdf -lnetcdff-g95/g" Mkinclude.bk > Mkinclude

make