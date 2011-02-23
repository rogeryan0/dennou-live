#!/bin/sh
#
PROGRAM=./gami_abcn_bench0_restart.out 
LOGFILE=./gami_abcn_bench0_restart.log

$PROGRAM << END_OF_DATA 1> $LOGFILE 2>&1
#
 &TINT    DELTA_T= 1.0D-005,  NSTEP= 20000     /
 &INITIAL INITIAL_FILE='', INITIAL_TIME= 0.0 /
 &OUTPUT  OUTPUT_FILE='gami_abcn_bench0_restart.nc', NDISP= 1 /
#
END_OF_DATA





