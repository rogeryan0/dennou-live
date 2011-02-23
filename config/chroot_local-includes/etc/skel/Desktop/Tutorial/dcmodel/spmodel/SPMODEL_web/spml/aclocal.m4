dnl= Definitions of Functions for configure
dnl
dnl  Authors::   Eizi TOYODA, Shin-ichi TAKEHIRO, Yasuhiro Morikawa, Youhei SASAKI
dnl  Version::   $Id: aclocal.m4,v 1.3 2007/10/09 15:06:03 uwabami Exp $
dnl  Tag Name::  $Name:  $
dnl  Copyright:: Copyright (C) GFD Dennou Club, 2000-2006. All rights reserved.
dnl  License::   See COPYRIGHT[link:COPYRIGHT]
dnl
dnl== Overview
dnl
dnl These functions are used by configure script.
dnl

dnl
dnl == DC_ARG_WITH(withname, description, varname, ifnot)
dnl
dnl
AC_DEFUN(DC_ARG_WITH, [
        AC_ARG_WITH($1, [$1: $2], [
                $3=$withval
        ], [
                AC_CACHE_CHECK([$2], $3, [$4])
        ])
])

dnl
dnl == DC_ARG_ENABLE(feature, description, varname, ifnot)
dnl
AC_DEFUN(DC_ARG_ENABLE, [
        AC_ARG_ENABLE($1, [  --enable-$1: $2], [
                $3=$enableval
        ], [
                AC_CACHE_CHECK([$2], $3, [$4])
        ])
])


dnl
dnl == Check libfile and set LIBDIR and  LIBNAME
dnl
dnl  Check existence of "libfile" file, and set LIBDIR, LIBNAME
dnl  from "libfile".
dnl
dnl  usage: DC_SET_LIBDIR_LIBNAME(libfile, LIBDIR, LIBNAME)
dnl
dnl
dnl == Check libfile and set LIBDIR and  LIBNAME
dnl
dnl  Check existence of "libfile" file, and set LIBDIR, LIBNAME
dnl  from "libfile". "libfile" file must have suffixes ".a" or ".so"
dnl
dnl  usage: DC_SET_LIBDIR_LIBNAME(libfile, LIBDIR, LIBNAME)
dnl
AC_DEFUN(DC_SET_LIBDIR_LIBNAME, [
        if test ! -f $1 ; then
                AC_MSG_ERROR(specified library file \"$1\" is not exist)
        fi
        $2=`dirname $1`
        case "$1" in
            *.a)
                $3=`basename $1 .a | sed 's/^lib//'`
                ;;
            *.so)
                $3=`basename $1 .so | sed 's/^lib//'`
                ;;
            *)
                AC_MSG_ERROR(specified library file \"$1\" have invalid suffix. Valid suffixes are \".a\" or \".so\")
                ;;
        esac
])

dnl
dnl == Set MODDIR from libfile
dnl
dnl Check existence of "libfile". And set MODDIR,
dnl
dnl usage: DC_SET_MODDIR(libfile, MODDIR)
dnl
AC_DEFUN(DC_SET_MODDIR, [
        if test ! -f $1 ; then
                AC_MSG_ERROR(specified library file \"$1\" is not exist)
        fi

        DC_SET_MODDIR_libdir=`dirname $1`
        DC_SET_MODDIR_libname=`basename $1 .a | sed 's/^lib//'`

        DC_SET_MODDIR_try1=""; DC_SET_MODDIR_try2=""
        if test -d ${DC_SET_MODDIR_try1:=$DC_SET_MODDIR_libdir/module} ; then
                $2=$DC_SET_MODDIR_try1
        elif test -d ${DC_SET_MODDIR_try2:=`dirname $DC_SET_MODDIR_libdir`/include} ; then
                $2=$DC_SET_MODDIR_try2
        else
                AC_MSG_ERROR($DC_SET_MODDIR_libname module directory not found)
        fi
])
