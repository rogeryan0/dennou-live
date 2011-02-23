#!/usr/bin/env bash
#
# plbaro_lfim_test1.1.f90 �Ѽ¹ԥ����륹����ץ�
#
#   - ${ROOTNAME} = ������ץȥե�����̾����ѥ����� /-[0-9].sh/ �������̾��
#   - ${NUMBER}   = ������ץȥե�����̾���� ${ROOTNAME}-[0-9].sh ���ֹ���ʬ
#
#   - ${ROOTNAME}.out ��¹Ԥ�����. ���ϥե������ ${ROOTNAME}-${NUMBER}.nc
#
#   - INPUTFILE �����ξ��ˤϥץ�����������Ȥ߹��ޤ줿����ͤ��Ȥ���. 
#
#   - �Ҥ��ĤŤ�����ȯŸ���³���������ˤ�, ${NUMBER} �� 1 �����䤷��
#     ������ץ�̾�Ȥ��ƥ��ԡ���, initial_time ������, �¹Ԥ�����ɤ�. 
#
#   - ��λ���˥᡼��Ǥ��Τ餻�������Ȥ��ˤ� EMAIL �����Τ�����Υ��ɥ쥹,
#     MAIL �˥᡼�륳�ޥ�ɤ����ꤹ��. 
#
#
# ����  2005/10/19 �ݹ����� ����
#
#
set -e                          # ���顼��������ץȤ��ߤޤ�
shopt -s extglob                # ��ĥ�ѥ�����ޥå���ͭ��

TIMECOMMAND=time                # ���ַ�¬���ޥ��

ROOTNAME=${0/-*([0-9]).sh/}
NUMBER=${0/$ROOTNAME-/} ; NUMBER=${NUMBER/.sh}

PROGRAM=${ROOTNAME}.out                    # �ᥤ��ץ����
NMLFILE=${ROOTNAME}.nml			   # NAMELIST �ե�����(�ѹ��Բ�)
LOGFILE=${ROOTNAME}-${NUMBER}.log	   # ��å��������ϥ��ե�����

INPUTFILE=   # ${ROOTNAME}-rst-$((${NUMBER}-1)).nc   # ���ϥǡ����ե�����
OUTPUTFILE=${ROOTNAME}-${NUMBER}.nc		     # ���ϥǡ����ե�����
RSTFILE=${ROOTNAME}-rst-${NUMBER}.nc                 # �ꥹ�����ȥǡ����ե�����

# �¸������ȥ�

TITLE="Test of linear terms of 2-dim barotropic model on a double-cyclic domain"


# ��λ���� e-mail �����Τ�����, �ʲ��򥳥��ȥ�����

#EMAIL=takepiro@gfd-dennou.org                  # �᡼�륢�ɥ쥹
#MAIL="rsh silicon mailx -s '$TITLE'"           # �����᡼�륳�ޥ��

if [ -e $NMLFILE ]; then
    rm $NMLFILE ;
    echo "Old $NMLFILE was removed." ; 
fi
cat << END_OF_DATA > $NMLFILE
#######################################################################
#
#----------------------- ��å�������٥����� ------------------------
#
# logical :: Verbose    ! ��Ĺ�ʥ�å��������� On/Off
# logical :: DebugOn    ! �ǥХå���å����� On/Off

 &message Verbose=.false., DebugOn=.false. / 

#---------------------------- ���������� -----------------------------
#
#  integer :: km                            ! X �����������ȿ�
#  integer :: lm                            ! Y �����������ȿ�
#  integer :: im                            ! X �����ʻ����� (>3*km)
#  integer :: jm                            ! Y �����ʻ����� (>3*lm)

 &gridset km=21, lm=21, im=64, jm=64 /

#----------------------------- ������ -------------------------------
#
# character(len=100) :: initial_file   ! ����ͥǡ����ե�����̾
#                                      ! (���ʤ������ǽ���ͤ�׻�)
# real               :: initial_time   ! �������

 &initial initial_file='$INPUTFILE', initial_time= 0.0   /

#------------------------------ ������ʬ ------------------------------
#
# real(8) :: delta_t          ! ������ʬ���
# integer :: nstep            ! ������ʬ���ƥå׿�

 &tint    delta_t= 1.0d-004,  nstep= 100000       /

#------------------------------ �������� ------------------------------
#
#  character(len=100) :: hst_file           ! �ҥ��ȥ꡼�ե�����̾
#  character(len=100) :: title              ! �����ȥ�
#  integer            :: hst_intstep        ! �ҥ��ȥ꡼���ϴֳ֥��ƥå׿�

 &history  hst_file='$OUTPUTFILE', hst_intstep= 2000, title='$TITLE' /

#------------------------ �ꥹ�����Ƚ������� --------------------------
#
#  character(len=100) :: rst_file           ! �ꥹ�����Ƚ��ϥե�����̾
#  integer            :: rst_intstep        ! �ꥹ�����Ƚ��ϴֳ֥��ƥå׿�

 &restart  rst_file='$RSTFILE', rst_intstep=1000 /

#------------------------ ������ե��륿������ --------------------------
#
#  real(8) :: TFiltCoef    = 0.05         ! ������ե��륿������
#  integer :: filt_intstep = 1            ! �ե��륿�����륹�ƥå״ֳ�

 &tfilter  TFiltCoef=0.05, filt_intstep=1 / 

#--------------------------- ʪ���ѿ����� ----------------------------
#  real(8)            :: XLength            ! �ΰ���礭��(X ����)
#  real(8)            :: YLength            ! �ΰ���礭��(Y ����)
#  real(8)            :: Beta               ! �¥ѥ�᥿��
#  integer            :: HVOrder            ! ĶǴ���μ���(1 �����̤�Ǵ��,
#                                           ! ��ʿ��ץ饷����γ���)
#  real(8)            :: HVisc              ! ĶǴ������

 &physics XLength=1.0D0, YLength=1.0D0, Beta=1.0, HVOrder=2, HVisc=1.0D-6 /

#---------------------------- �¸��ѥ�᥿�� ----------------------------
# ����� 1 �ȿ���ʬ�Τ�
#
# integer :: KInitial             ! �ȿ� X ��ʬ
# integer :: LInitial             ! �ȿ� Y ��ʬ
# real(8) :: Amplitude            ! ����

 &initwave KInitial=3, LInitial=2, Amplitude=1.0D0 /

######################################################################
END_OF_DATA

echo Program started at `date` > $LOGFILE
($TIMECOMMAND $PROGRAM) 1>> $LOGFILE 2>&1 || true
rm $NMLFILE
echo Program ended at `date` >> $LOGFILE

# ��λ���� e-mail �����Τ�����, �ʲ��򥳥��ȥ�����
#$MAIL $EMAIL < $LOGFILE
