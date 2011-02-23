#!/bin/sh
#----------------------------------------------------------------------
#     Copyright (c) 2003 Masatsugu Odaka. All rights reserved.
#----------------------------------------------------------------------
# Shell script for sample program for gtool_history/gtool4 and ISPACK  
# 2003/06/18 M. Odaka
#

PROGRAM=./shallow_sfvp
LOGFILE=./shallow_sfvp.log
NMFILE=indata.nm


if [ ! -f $PROGRAM ] ; then 
    echo "$PROGRAM is not found."
    exit 1
elif [ ! -f $NMFILE ] ; then
    echo "$NMFILE is not found."
    exit 1
else 
    $PROGRAM < $NMFILE 1 > $LOGFILE 2>& 1
    exit 0
fi

