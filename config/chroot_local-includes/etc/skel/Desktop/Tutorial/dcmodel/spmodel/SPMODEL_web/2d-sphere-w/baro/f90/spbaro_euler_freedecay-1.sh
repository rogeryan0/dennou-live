#!/usr/bin/env bash
#
# spbaro_euler_freedecay.f90 $BMQ<B9T%7%'%k%9%/%j%W%H(B
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
# $BMzNr(B  2005/04/05 $BC]9-??0l(B $B:n@.(B
#       2007/12/07 $BC]9-??0l(B $B%3%^%s%I0z?t$r2r@O$7(B, NAMELIST $B%U%!%$%kL>$r<hF@(B
#
#
shopt -s extglob                # $B3HD%%Q%?!<%s%^%C%A%s%0M-8z(B

TIMECOMMAND=time                # $B;~4V7WB,%3%^%s%I(B

ROOTNAME=${0/-*([0-9]).sh/}
NUMBER=${0/$ROOTNAME-/} ; NUMBER=${NUMBER/.sh}

PROGRAM=${ROOTNAME}.out
NMLFILE=${ROOTNAME}-${NUMBER}.nml		   # NAMELIST $B%U%!%$%k(B
LOGFILE=${ROOTNAME}-${NUMBER}.log
INPUTFILE=           # ${ROOTNAME}-$((${NUMBER}-1)).nc
OUTPUTFILE=${ROOTNAME}-${NUMBER}.nc

TITLE="Decaying turbulence problem of 2-dim barotropic fluid on a rotating sphere (T85)"

EMAIL=
MAIL=
#EMAIL=hogehoge@gfd-dennou.org
#MAIL="mailx -s '$TITLE'"

if [ -e $NMLFILE ]; then
    rm $NMLFILE ;
    echo "Old $NMLFILE was removed." ; 
fi
cat << END_OF_DATA > $NMLFILE
#######################################################################
#
#----------------------- $B%a%C%;!<%8%l%Y%k@_Dj(B ------------------------
#
# logical :: Verbose    ! $B>iD9$J%a%C%;!<%8=PNO(B On/Off
# logical :: DebugOn    ! $B%G%P%C%0%a%C%;!<%8(B On/Off

 &message Verbose=.false., DebugOn=.false. /

#---------------------------- $B2rA|EY@_Dj(B -----------------------------
#
# integer :: nm         ! ($B;03Q7A(B)$B@ZCGA4GH?t(B
# integer :: im         ! $B7PEYJ}8~3J;RE@?t(B (>3*nm+1)
# integer :: jm         ! $B0^EYJ}8~3J;RE@?t(B (>3*nm/2)

# &gridset nm=10,  im=32,   jm=16 /
# &gridset nm=21,  im=64,   jm=32 /
# &gridset nm=42,  im=128,  jm=64 / 
 &gridset nm=85,  im=256,  jm=128 / 
# &gridset nm=170, im=512,  jm=256 / 
# &gridset nm=341, im=1024, jm=512 / 

#----------------------------- $B=i4|>r7o(B -------------------------------
#
# character(len=100) :: initial_file   ! $B=i4|CM%G!<%?%U%!%$%kL>(B
#                                      ! ($B6u$J$iFbIt$G=i4|CM$r7W;;(B)
# real               :: initial_time   ! $B=i4|;~9o(B

 &initial initial_file='$INPUTFILE', initial_time= 0.0   /

#------------------------------ $B;~4V@QJ,(B ------------------------------
#
# real(8) :: delta_t          ! $B;~4V@QJ,9o$_(B
# integer :: nstep            ! $B;~4V@QJ,%9%F%C%W?t(B

 &tint    delta_t= 1.0d-004,  nstep=50000       /

#--------------------------- $BJ*M}JQ?t@_Dj(B ----------------------------
#
#  real(8)   :: Radius             ! $B5e$NH>7B(B
#  real(8)   :: Omega              ! $B2sE>3QB.EY(B
#  integer   :: HVOrder            ! $BD6G4@-$N<!?t(B(1 $B$GIaDL$NG4@-(B,
#                                  ! $B?eJ?%i%W%i%7%"%s$N3,?t(B)
#  real(8)   :: HVisc              ! $BD6G4@-78?t(B

 &physics  Radius=1.0D0, Omega=400.0D0, HVOrder=8, HVisc=1.0D-27 / 

#------------------------------ $B=PNO@_Dj(B ------------------------------
#
#  character(STRING)  :: output_file        ! $B=PNO%U%!%$%kL>(B
#  character(STRING)  :: title              ! $B%?%$%H%k(B
#  integer            :: ndisp              ! $B=PNO4V3V%9%F%C%W?t(B

 &output  output_file='$OUTPUTFILE', NDISP=1000, title='$TITLE' /

#---------------------------- $B=i4|CM@_Dj(B ----------------------------
#
#  integer    :: seed=0             ! seed(1)$B$K@_Dj$9$k<o$NCM(B
#  integer    :: nzero=10           ! $B=i4|%(%M%k%.!<%9%Z%/%H%kJ,I[$N%Q%i%a%?(B
#  integer    :: gamma=100          ! $B=i4|%(%M%k%.!<%9%Z%/%H%kJ,I[$N%Q%i%a%?(B
#  real(8)    :: Etotal=1.0D0       ! $B=i4|A4%(%M%k%.!<$NCM(B
#  character(len=10) :: disttype='YIHY2002' ! $B=i4|%(%M%k%.!<J,I[$N%?%$%W(B
#                                            ! (YIHY2002/YY1993)

 &initvalue seed=$NUMBER, nzero=40, gamma=100, Etotal=1.0, disttype='YIHY2002'/

######################################################################
END_OF_DATA

echo Program started at `date` > $LOGFILE
($TIMECOMMAND $PROGRAM $NMLFILE) 1>> $LOGFILE 2>&1 || true
rm $NMLFILE
echo Program ended at `date` >> $LOGFILE

if [ -n "$EMAIL" ]; then
    $MAIL $EMAIL < $LOGFILE
fi
