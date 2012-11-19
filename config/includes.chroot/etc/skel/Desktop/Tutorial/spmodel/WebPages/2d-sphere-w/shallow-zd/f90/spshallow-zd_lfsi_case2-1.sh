#!/usr/bin/env bash
#
# baro_shallow_zd_lsfi_case2.f90 �Ѽ¹ԥ����륹����ץ�
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
# ����  2005/09/27 �ݹ����� ����
#       2005/09/28 �ݹ����� NAMELIST �ѿ��ѹ�
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

TITLE="Test for 2-dim shallow water fluid on a rotating sphere (case2)"


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
# logical :: Verbose=.false.          ! ���ϥ�å�������٥�
# logical :: DebugOn=.false.          ! �ǥХå����ϥ���ȥ���

 &message Verbose=.false., DebugOn=.false. / 

#---------------------------- ���������� -----------------------------
#
# integer :: nm             ! (���ѷ�)�������ȿ�
# integer :: im             ! ���������ʻ����� ( > 3*nm + 1)
# integer :: jm             ! ���������ʻ����� ( > 3*nm/2 )

# &gridset nm=10,  im=32,   jm=16 /
# &gridset nm=21,  im=64,   jm=32 /
 &gridset nm=42,  im=128,  jm=64 / 
# &gridset nm=85,  im=256,  jm=128 / 
# &gridset nm=170, im=512,  jm=256 / 
# &gridset nm=341, im=1024, jm=512 / 

#----------------------------- ������ -------------------------------
#
#  character(len=100) :: initial_file  ! ����ͥǡ����ե�����̾
#                                      ! (���ʤ������ǽ���ͤ�׻�)
#  real               :: initial_time  ! �������

 &initial initial_file='$INPUTFILE', initial_time= 0.0   /

#------------------------------ ������ʬ ------------------------------
#
#  real(8) :: delta_t   ! ������ʬ���
#  integer :: nstep     ! ������ʬ���ƥå׿�

 &tint    delta_t= 1200D0,  nstep= 360     /

#-------------------------- ������ե��륿�� --------------------------
#
# real(8) :: TFiltCoef         ! ������ե��륿������
# integer :: filt_intstep      ! �ե��륿�����륹�ƥå״ֳ�

 &tfilter TFiltCoef=0.0D0,   filt_intstep=0 /       # �ե��륿���򤫤��ʤ�
# &tfilter TFiltCoef=0.05D0, filt_intstep=1 /     # ��󤫤���

#------------------------------ �������� ------------------------------
#
# character(len=100) :: hst_file        ! �ҥ��ȥ꡼�ե�����̾
# character(len=100) :: title           ! �����ȥ�
# integer            :: hst_intstep     ! �ҥ��ȥ꡼���ϴֳ֥��ƥå׿�

 &history  hst_file='$OUTPUTFILE', hst_intstep= 10, title='$TITLE' /

#------------------------ �ꥹ�����Ƚ������� --------------------------
#
# character(len=100) :: rst_file         ! �ꥹ�����Ƚ��ϥե�����̾
# integer            :: rst_intstep      ! �ꥹ�����Ƚ��ϴֳ֥��ƥå׿�

 &restart  rst_file='$RSTFILE', rst_intstep=400 /

#--------------------------- ʪ���ѿ����� ----------------------------
#
#  real(8) :: Radius    ! ����Ⱦ��
#  real(8) :: Omega     ! ��ž��®��
#  real(8) :: Alpha     ! ��ž������(�ˤȤγ���,deg.)
#  real(8) :: Grav      ! ���ϲ�®��
#  real(8) :: Hbar      ! ʿ�ѿ忼
#  integer :: HVOrder   ! ĶǴ���μ���
#                       ! (��ʿ��ץ饷����γ���)
#  real(8) :: HVisc     ! ĶǴ������
#  integer :: HDOrder   ! Ķ�Ȼ��μ���
#                       ! (��ʿ��ץ饷����γ���)
#  real(8) :: HDiff     ! Ķ�Ȼ�����

 &physics  Radius=6.37122D6, Omega=7.292D-5, Alpha=90.0, 
           Grav=9.80616, Hbar = 3.0D3, 
           HVOrder=2, HVisc=0.5D16, HDOrder=2, HDiff=0.5D16       /

#---------------------------- �¸��ѥ�᥿�� ----------------------------
#
# real(8) :: U0         ! ���β�žή��®��(m/sec)

 &case2  U0 = 3.861068D1 /

######################################################################
END_OF_DATA

echo Program started at `date` > $LOGFILE
($TIMECOMMAND $PROGRAM) 1>> $LOGFILE 2>&1 || true
rm $NMLFILE
echo Program ended at `date` >> $LOGFILE

# ��λ���� e-mail �����Τ�����, �ʲ��򥳥��ȥ�����
#$MAIL $EMAIL < $LOGFILE
