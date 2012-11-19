#!/usr/bin/env bash
#
# ɽ��  ��ž��̥֥��ͥ��������ʥ��ǥ�
#
#       -- �٥���ޡ����¹��ѥ����륹����ץ�
#
#   ������ץ�̾�η��� : ${ROOTNAME}-${NUMBER}.sh
#
#   - ${ROOTNAME} = ������ץȥե�����̾����
#                   �ѥ����� /-*([0-9])-*([0-9]).sh/ �������̾��
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
#   - ��λ���˥᡼��Ǥ��Τ餻�������Ȥ��ˤ� EMAIL �����Τ�����Υ��ɥ쥹,
#     MAIL �˥᡼�륳�ޥ�ɤ����ꤹ��. 
#
# ����  2007/09/13  �ݹ�����    ����������
#       2007/12/20  �ݹ�����    ����������
#
#
shopt -s extglob                # ��ĥ�ѥ�����ޥå���ͭ��

#TIMECOMMAND=timex               # ���ַ�¬���ޥ��
TIMECOMMAND=/usr/bin/time               # ���ַ�¬���ޥ��

ROOTNAME=${0/-*([0-9]).sh/}
NUMBER=${0/$ROOTNAME-/} ; NUMBER=${NUMBER/.sh}

PROGRAM=${ROOTNAME}.out
NMLFILE=${ROOTNAME}-${NUMBER}.nml  # NAMELIST �ե�����
LOGFILE=${ROOTNAME}-${NUMBER}.log
INPUTFILE=${ROOTNAME}-$((${NUMBER}-1)).nc
OUTPUTFILE=${ROOTNAME}-${NUMBER}.nc

# �¸������ȥ�

TITLE="dynamo benchmark (case1) (T42L32)"

# ��λ���� e-mail �����Τ�����, �ʲ��� EMAIL �˰����
# �᡼�륢�ɥ쥹�ȥ᡼���������ޥ�ɤ���ꤷ�����Ȥ򳰤�.
EMAIL=
#EMAIL=takepiro@gfd-dennou.org
#MAIL="rsh silicon mailx -s '$TITLE'"

if [ -e $NMLFILE ]; then
    rm $NMLFILE ;
    echo "Old $NMLFILE was removed." ; 
fi
cat << END_OF_DATA > $NMLFILE
#######################################################################
#
# ��å���������ȥ���
#    verbose = .ture. �� namelist �����Ϥ�¥����å����������Ϥ����.
#
 &message verbose=.false., DebugOn=.false. /
# &message verbose=.false., DebugOn=.true. /

#
# ����������
#    nm:��ʿ�������ȿ�, lm:��ľ�����ӥ����պ��缡��
#    im: ���������ʻ�����, jm: ���������ʻ�����, km: ư�������ʻ�����
#
# &gridset nm=10,  lm=8,  im=32,   jm=16,  km=8  /
# &gridset nm=10,  lm=16, im=32,   jm=16,  km=16 /
# &gridset nm=21,  lm=16, im=64,   jm=32,  km=16 /
# &gridset nm=21,  lm=24, im=64,   jm=32,  km=24 /
# &gridset nm=21,  lm=32, im=64,   jm=32,  km=32 /
 &gridset nm=42,  lm=32, im=128,  jm=64,  km=32 /
# &gridset nm=42,  lm=48, im=128,  jm=64,  km=48 /
# &gridset nm=85,  lm=32, im=256,  jm=128, km=32 /
# &gridset nm=85,  lm=48, im=256,  jm=128, km=48 /
# &gridset nm=85,  lm=64, im=256,  jm=128, km=64 /
# &gridset nm=170, lm=48, im=512,  jm=256, km=48 /
# &gridset nm=170, lm=64, im=512,  jm=256, km=64 /
# &gridset nm=341, lm=80, im=1024, jm=512, km=80 /

#
# ����⳰Ⱦ������
#   eta : ����⳰Ⱦ����, ri : ��¦���Ⱦ��, ro: ��¦���Ⱦ��
#   eta �Τ�Ϳ����ȼ�ưŪ�� ro-ri=1 �Ȥʤ�褦�� ro, ri �����ꤵ���. 
#
 &radius  eta=0.35 /

#
# ���������
#    initial_file : ����ͥե�����. ����ʤ�������ǽ���ͤ�Ϳ������
#                   (�Ż߾���, ��̿���� 1 �������پ�)
#    initial_time : ����ͤλ���
#
 &initial initial_file='$INPUTFILE', initial_time= 1.0   /

#
#  ������ʬ����
#       delta_t : ���ֹ��
#       nstep   : ��׻����ƥå׿�
#
 &tint    delta_t=1.0d-004,  nstep= 100000    /

#
# ��������
#    output_file : ���ϥե�����̾
#    ndisp       : ���ϻ��֥��ƥå״ֳ�
#    title       : �ե�����˽񤭹��ޤ��¸�̾
#
 &output  output_file='$OUTPUTFILE', NDISP= 5000, title='$TITLE' /

#
# ʪ���ѥ�᥿������
#    Ra     : �쥤�꡼��
#    Pr     : �ץ��ɥ��
#    Ta     : �ƥ��顼��
#    Pm     : �����ץ��ɥ��
#
 &physics Ra=100.0d0, Pr=1.0d0, Ekman=1.0d-3, Pm=5.0d0 /

#
# �����������
#     VelBC  : ®�ٶ������
#              FF : ξü��ͳ���٤�
#              FR : ��ü��ͳ���٤�, ��üǴ��
#              RF : ��üǴ��, ��ü��ͳ���٤�
#              RR : ξüǴ��
#
#     TempBC : ���پ��𶭳����(�ͤ� 0 �����ꤵ���)
#              DD : ξü���ٸ���
#              DN : ��ü���ٸ���, ��ü���ٷ��ٸ���
#              ND : ��ü���ٷ��ٸ���, ��ü���ٸ���
#              NN : ξü���ٷ��ٸ���
#
 &boundary VelBC='RR', TempBC='DD', 
           Temptop=0.0d0, Tempbottom=1.0D0 /
#           Temptop=0.0d0, Tempbottom=-1.66666666666666666666 /
#                             dT/dr = -1/eta at r=ri

#
# ����ͤμ���
#      case=0 : ����ʤ��¸�
#      case=1 : ���줢��¸�
#
 &inittype case=1 / 

######################################################################
END_OF_DATA

echo Program started at `date` on `hostname` > $LOGFILE
echo $TIMECOMMAND $PROGRAM $NMLFILE
($TIMECOMMAND $PROGRAM $NMLFILE) 1>> $LOGFILE 2>&1 || true
rm $NMLFILE
echo Program ended at `date` >> $LOGFILE

# ��λ���� e-mail �����Τ���. 
if [ -n "$EMAIL" ]; then
    $MAIL $EMAIL < $LOGFILE
fi
