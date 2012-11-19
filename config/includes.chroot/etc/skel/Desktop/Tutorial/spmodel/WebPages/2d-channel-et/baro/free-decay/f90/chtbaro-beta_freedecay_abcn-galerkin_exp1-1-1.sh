#!/usr/bin/env bash
#
# chtbaro_freedecay_rk2cn-galerlin_exp1.f90 �Ѽ¹ԥ����륹����ץ�
#
#   ������ץ�̾�η��� : ${ROOTNAME}-${SEED}-${NUMBER}.sh
#
#   - ${ROOTNAME} = ������ץȥե�����̾����
#                   �ѥ����� /-*([0-9])-*([0-9]).sh/ �������̾��
#   - ${SEED}     = ������ץ�̾�� 1 �Ĥ���ֹ���ʬ. 
#                   ���������ͤμ�Ȥ��ƻȤ���
#   - ${NUMBER}   = ������ץ�̾�� 2 �Ĥ���ֹ���ʬ. 
#                   �ꥹ�����ȥե������ȤäƷ�³�׻�����Ȥ����ֹ�. 
#
#   - ${ROOTNAME}.out ��¹Ԥ�����. 
#
#   - INPUTFILE �����ξ��ˤϥץ�����������Ȥ߹��ޤ줿����ͤ��Ȥ���. 
#
#   - �Ҥ��ĤŤ�����ȯŸ���³���������ˤ�, ${NUMBER} �� 1 �����䤷��
#     ������ץ�̾�Ȥ��ƥ��ԡ���, initial_time ������, �¹Ԥ�����ɤ�. 
#
#   - �ۤʤ����ͤ���η׻�(���󥵥�֥���С�)�򤹤���ˤ�, 
#     ${SEED} ����ʬ���ֹ���̤��ͤˤ���������ץȤ˥��ԡ����Ƽ¹Ԥ�����ɤ�.
#
#   - ��λ���˥᡼��Ǥ��Τ餻�������Ȥ��ˤ� EMAIL �����Τ�����Υ��ɥ쥹,
#     MAIL �˥᡼�륳�ޥ�ɤ����ꤹ��. 
#
#
# ����  2005/10/31 �ݹ����� ����
#       2007/11/30 �ݹ����� ���ޥ�ɰ�������Ϥ�, NAMELIST �ե�����̾�����
#       2007/12/02 �������� �ץ����̾, EMAIL �����ѹ�
#
#
set -e                          # ���顼��������ץȤ��ߤޤ�
shopt -s extglob                # ��ĥ�ѥ�����ޥå���ͭ��

TIMECOMMAND=time                # ���ַ�¬���ޥ��


ROOTNAME=${0/-*([0-9])-*([0-9]).sh/}
NUMBER=${0/$ROOTNAME-/} ; SEED=${NUMBER/-*([0-9]).sh/} 
NUMBER=${NUMBER/$SEED-/} ; NUMBER=${NUMBER/.sh/}

PROGRAM=./${ROOTNAME}.out                    # �ᥤ��ץ����
NMLFILE=${ROOTNAME}-${SEED}-${NUMBER}.nml  # NAMELIST �ե�����
LOGFILE=${ROOTNAME}-${SEED}-${NUMBER}.log  # ��å��������ϥ��ե�����

INPUTFILE=''                               # �����ǽ���������
                                           # ������ꥹ�����ȥե�����
INPUTFILE= #${ROOTNAME}-${SEED}-rst-$((${NUMBER}-1)).nc 
OUTPUTFILE=${ROOTNAME}-${SEED}-${NUMBER}.nc

OUTPUTFILE=${ROOTNAME}-${SEED}-${NUMBER}.nc          # ���ϥǡ����ե�����
RSTFILE=${ROOTNAME}-${SEED}-rst-${NUMBER}.nc         # �ꥹ�����ȥǡ����ե�����

# �¸������ȥ�

TITLE="Free-decay turebulence of 2-dim barotropic fluid on a channel domain"



# ��λ���� e-mail �����Τ�����, �ʲ��� EMAIL �˰����
# �᡼�륢�ɥ쥹����ꤷ, �����ȥ����Ȥ򳰤�.

#EMAIL=hogehoge@gfd-dennou.org                 # �᡼�륢�ɥ쥹
#EMAIL=${USER}@gfd-dennou.org                  # �᡼�륢�ɥ쥹
MAIL="rsh silicon mailx -s '$TITLE'"           # �����᡼�륳�ޥ��

if [ -e $NMLFILE ]; then
    rm $NMLFILE ;
    echo "Old $NMLFILE was removed." ; 
fi
echo $NMLFILE
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

# &gridset km=21, lm=21, im=64, jm=64 /
# &gridset km=42, lm=42, im=128, jm=128 /
# &gridset km=85, lm=85, im=256, jm=256 /
 &gridset km=85, lm=170, im=256, jm=512 /
# &gridset km=170, lm=170, im=512, jm=512 /

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

 &restart  rst_file='$RSTFILE', rst_intstep=1000000 /

#--------------------------- ʪ���ѿ����� ----------------------------
#  real(8)            :: XLength            ! �ΰ���礭��(X ����)
#  real(8)            :: YLength            ! �ΰ���礭��(Y ����)
#  real(8)            :: Beta               ! �¥ѥ�᥿��
#  real(8)            :: Visc=1.0D0         ! Ǵ������
#  character(len=2)   :: velBC              ! ή�춭�����(RR/RS/SR/SS)

 &physics XLength=6.2831853D0, YLength=6.2831853D0, 
          Beta=100.0, Visc=2.0D-4, velBC='SS' /

#---------------------------- �¸��ѥ�᥿�� ----------------------------
#  integer    :: Seed               ! seed(1)�����ꤹ������
#  real(8)    :: Nmin               ! ������ͥ륮��ʬ�ۤ����ȿ��ΰ��Ǿ���
#  real(8)    :: Nmax               ! ������ͥ륮��ʬ�ۤ����ȿ��ΰ������
#  real(8)    :: Etotal             ! ���ʿ�ѥ��ͥ륮������

 &initvalue Seed=$SEED, Nmin=38.0D0, Nmax=40.0D0, Etotal=1.0D0 /

######################################################################
END_OF_DATA

echo Program started at `date` > $LOGFILE
($TIMECOMMAND $PROGRAM $NMLFILE) 1>> $LOGFILE 2>&1 || true
rm $NMLFILE
echo Program ended at `date` >> $LOGFILE

# ��λ���� e-mail �����Τ�����, �ʲ��򥳥��ȥ�����
if [ -n "$EMAIL" ]; then
    $MAIL $EMAIL < $LOGFILE
fi
