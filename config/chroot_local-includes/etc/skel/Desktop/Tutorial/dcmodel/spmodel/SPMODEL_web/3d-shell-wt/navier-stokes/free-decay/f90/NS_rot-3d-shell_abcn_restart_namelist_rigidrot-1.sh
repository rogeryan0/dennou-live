#!/usr/bin/env bash
#
# NS_rot-3d-shell_abcn_restart_namelist_rigidrot.f90 $BMQ<B9T%7%'%k%9%/%j%W%H(B
#
#   - ${ROOTNAME} = $B%9%/%j%W%H%U%!%$%kL>$+$i%Q%?!<%s(B /-[0-9].sh/ $B$r=|$$$?L>A0(B
#   - ${NUMBER}   = $B%9%/%j%W%H%U%!%$%kL>$+$i(B ${ROOTNAME}-[0-9].sh $B$NHV9fItJ,(B
#
#   - ${ROOTNAME}.out $B$r<B9T$7$9$k(B. $B=PNO%U%!%$%k$O(B ${ROOTNAME}-${NUMBER}.nc
#
#   - INPUTFILE $B$,6u$N>l9g$K$O%W%m%0%i%`FbIt$KAH$_9~$^$l$?=i4|CM$,;H$o$l$k(B. 
#
#   - $B$R$-$D$E$-;~4VH/E8$r7QB3$7$?$$>l9g$K$O(B, ${NUMBER} $B$r(B 1 $B$DA}$d$7$?(B
#     $B%9%/%j%W%HL>$H$7$F%3%T!<$7(B, initial_time $B$r=$@5$7(B, $B<B9T$9$l$PNI$$(B. 
#
#   - $B=*N;;~$K%a!<%k$G$*CN$i$;$7$?$$$H$-$K$O(B EMAIL $B$KDLCN$9$k@h$N%"%I%l%9(B,
#     MAIL $B$K%a!<%k%3%^%s%I$r@_Dj$9$k(B. 
#
#
# $BMzNr(B  2005/02/11 $BC]9-(B $B??0l(B $B:n@.(B
#       2005/02/19 $BC]9-(B $B??0l(B $B9dBN2sE>=i4|CMMQ$K2~B$(B
#
#
shopt -s extglob                # $B3HD%%Q%?!<%s%^%C%A%s%0M-8z(B

TIMECOMMAND=time                # $B;~4V7WB,%3%^%s%I(B

ROOTNAME=${0/-*([0-9]).sh/}
NUMBER=${0/$ROOTNAME-/} ; NUMBER=${NUMBER/.sh}

PROGRAM=${ROOTNAME}.out
LOGFILE=${ROOTNAME}-${NUMBER}.log
INPUTFILE=           # ${ROOTNAME}-$((${NUMBER}-1)).nc
OUTPUTFILE=${ROOTNAME}-${NUMBER}.nc

TITLE="Rigid rotation in a spherical shell"

#EMAIL=takepiro@gfd-dennou.org
#MAIL="rsh silicon mailx -s '$TITLE'"

echo Program started at `date` > $LOGFILE
($TIMECOMMAND $PROGRAM) << END_OF_DATA 1>> $LOGFILE 2>&1
#######################################################################
 &message verbose=.false. /
 &radius  eta=0.6 /

 &initial initial_file='$INPUTFILE', initial_time= 0.0   /
 &tint    delta_t= 2.0d-004,  nstep= 1000       /

 &output  output_file='$OUTPUTFILE', NDISP= 50, title='$TITLE' /

 &physics Rs=1.0D-1, Re=0.0D0 / 

 &boundary VelBC='FF' / 

 &rotation Omega=1.0D0, LonOmega=240.0, LatOmega=0.0 /

######################################################################
END_OF_DATA

echo Program ended at `date` >> $LOGFILE

#$MAIL $EMAIL < $LOGFILE
