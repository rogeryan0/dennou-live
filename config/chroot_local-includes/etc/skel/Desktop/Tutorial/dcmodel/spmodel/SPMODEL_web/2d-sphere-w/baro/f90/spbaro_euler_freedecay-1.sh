#!/usr/bin/env bash
#
# spbaro_euler_freedecay.f90 用実行シェルスクリプト
#
#   - ${ROOTNAME} = スクリプトファイル名からパターン /-[0-9].sh/ を除いた名前
#   - ${NUMBER}   = スクリプトファイル名から ${ROOTNAME}-[0-9].sh の番号部分
#
#   - ${ROOTNAME}.out を実行しする. 出力ファイルは ${ROOTNAME}-${NUMBER}.nc
#
#   - INPUTFILE が空の場合にはプログラム内部に組み込まれた初期値が使われる. 
#
#   - ひきつづき時間発展を継続したい場合には, ${NUMBER} を 1 つ増やした
#     スクリプト名としてコピーし, initial_time を修正し, 実行すれば良い. 
#
#   - 終了時にメールでお知らせしたいときには EMAIL に通知する先のアドレス,
#     MAIL にメールコマンドを設定する. 
#
#
# 履歴  2005/04/05 竹広真一 作成
#       2007/12/07 竹広真一 コマンド引数を解析し, NAMELIST ファイル名を取得
#
#
shopt -s extglob                # 拡張パターンマッチング有効

TIMECOMMAND=time                # 時間計測コマンド

ROOTNAME=${0/-*([0-9]).sh/}
NUMBER=${0/$ROOTNAME-/} ; NUMBER=${NUMBER/.sh}

PROGRAM=${ROOTNAME}.out
NMLFILE=${ROOTNAME}-${NUMBER}.nml		   # NAMELIST ファイル
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
#----------------------- メッセージレベル設定 ------------------------
#
# logical :: Verbose    ! 冗長なメッセージ出力 On/Off
# logical :: DebugOn    ! デバッグメッセージ On/Off

 &message Verbose=.false., DebugOn=.false. /

#---------------------------- 解像度設定 -----------------------------
#
# integer :: nm         ! (三角形)切断全波数
# integer :: im         ! 経度方向格子点数 (>3*nm+1)
# integer :: jm         ! 緯度方向格子点数 (>3*nm/2)

# &gridset nm=10,  im=32,   jm=16 /
# &gridset nm=21,  im=64,   jm=32 /
# &gridset nm=42,  im=128,  jm=64 / 
 &gridset nm=85,  im=256,  jm=128 / 
# &gridset nm=170, im=512,  jm=256 / 
# &gridset nm=341, im=1024, jm=512 / 

#----------------------------- 初期条件 -------------------------------
#
# character(len=100) :: initial_file   ! 初期値データファイル名
#                                      ! (空なら内部で初期値を計算)
# real               :: initial_time   ! 初期時刻

 &initial initial_file='$INPUTFILE', initial_time= 0.0   /

#------------------------------ 時間積分 ------------------------------
#
# real(8) :: delta_t          ! 時間積分刻み
# integer :: nstep            ! 時間積分ステップ数

 &tint    delta_t= 1.0d-004,  nstep=50000       /

#--------------------------- 物理変数設定 ----------------------------
#
#  real(8)   :: Radius             ! 球の半径
#  real(8)   :: Omega              ! 回転角速度
#  integer   :: HVOrder            ! 超粘性の次数(1 で普通の粘性,
#                                  ! 水平ラプラシアンの階数)
#  real(8)   :: HVisc              ! 超粘性係数

 &physics  Radius=1.0D0, Omega=400.0D0, HVOrder=8, HVisc=1.0D-27 / 

#------------------------------ 出力設定 ------------------------------
#
#  character(STRING)  :: output_file        ! 出力ファイル名
#  character(STRING)  :: title              ! タイトル
#  integer            :: ndisp              ! 出力間隔ステップ数

 &output  output_file='$OUTPUTFILE', NDISP=1000, title='$TITLE' /

#---------------------------- 初期値設定 ----------------------------
#
#  integer    :: seed=0             ! seed(1)に設定する種の値
#  integer    :: nzero=10           ! 初期エネルギースペクトル分布のパラメタ
#  integer    :: gamma=100          ! 初期エネルギースペクトル分布のパラメタ
#  real(8)    :: Etotal=1.0D0       ! 初期全エネルギーの値
#  character(len=10) :: disttype='YIHY2002' ! 初期エネルギー分布のタイプ
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
