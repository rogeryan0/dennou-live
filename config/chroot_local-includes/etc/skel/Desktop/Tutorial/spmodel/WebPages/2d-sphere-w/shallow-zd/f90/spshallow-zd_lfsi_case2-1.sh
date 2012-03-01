#!/usr/bin/env bash
#
# baro_shallow_zd_lsfi_case2.f90 用実行シェルスクリプト
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
# 履歴  2005/09/27 竹広真一 作成
#       2005/09/28 竹広真一 NAMELIST 変数変更
#
set -e                          # エラー時スクリプトが止まる
shopt -s extglob                # 拡張パターンマッチング有効

TIMECOMMAND=time                # 時間計測コマンド

ROOTNAME=${0/-*([0-9]).sh/}
NUMBER=${0/$ROOTNAME-/} ; NUMBER=${NUMBER/.sh}

PROGRAM=${ROOTNAME}.out                    # メインプログラム
NMLFILE=${ROOTNAME}.nml			   # NAMELIST ファイル(変更不可)
LOGFILE=${ROOTNAME}-${NUMBER}.log	   # メッセージ出力ログファイル

INPUTFILE=   # ${ROOTNAME}-rst-$((${NUMBER}-1)).nc   # 入力データファイル
OUTPUTFILE=${ROOTNAME}-${NUMBER}.nc		     # 出力データファイル
RSTFILE=${ROOTNAME}-rst-${NUMBER}.nc                 # リスタートデータファイル

# 実験タイトル

TITLE="Test for 2-dim shallow water fluid on a rotating sphere (case2)"


# 終了時に e-mail で通知する場合, 以下をコメントアウト

#EMAIL=takepiro@gfd-dennou.org                  # メールアドレス
#MAIL="rsh silicon mailx -s '$TITLE'"           # 送信メールコマンド

if [ -e $NMLFILE ]; then
    rm $NMLFILE ;
    echo "Old $NMLFILE was removed." ; 
fi
cat << END_OF_DATA > $NMLFILE
#######################################################################
#
#----------------------- メッセージレベル設定 ------------------------
#
# logical :: Verbose=.false.          ! 出力メッセージレベル
# logical :: DebugOn=.false.          ! デバッグ出力コントロール

 &message Verbose=.false., DebugOn=.false. / 

#---------------------------- 解像度設定 -----------------------------
#
# integer :: nm             ! (三角形)切断全波数
# integer :: im             ! 経度方向格子点数 ( > 3*nm + 1)
# integer :: jm             ! 緯度方向格子点数 ( > 3*nm/2 )

# &gridset nm=10,  im=32,   jm=16 /
# &gridset nm=21,  im=64,   jm=32 /
 &gridset nm=42,  im=128,  jm=64 / 
# &gridset nm=85,  im=256,  jm=128 / 
# &gridset nm=170, im=512,  jm=256 / 
# &gridset nm=341, im=1024, jm=512 / 

#----------------------------- 初期条件 -------------------------------
#
#  character(len=100) :: initial_file  ! 初期値データファイル名
#                                      ! (空なら内部で初期値を計算)
#  real               :: initial_time  ! 初期時刻

 &initial initial_file='$INPUTFILE', initial_time= 0.0   /

#------------------------------ 時間積分 ------------------------------
#
#  real(8) :: delta_t   ! 時間積分刻み
#  integer :: nstep     ! 時間積分ステップ数

 &tint    delta_t= 1200D0,  nstep= 360     /

#-------------------------- タイムフィルター --------------------------
#
# real(8) :: TFiltCoef         ! タイムフィルター係数
# integer :: filt_intstep      ! フィルターするステップ間隔

 &tfilter TFiltCoef=0.0D0,   filt_intstep=0 /       # フィルターをかけない
# &tfilter TFiltCoef=0.05D0, filt_intstep=1 /     # 毎回かける

#------------------------------ 出力設定 ------------------------------
#
# character(len=100) :: hst_file        ! ヒストリーファイル名
# character(len=100) :: title           ! タイトル
# integer            :: hst_intstep     ! ヒストリー出力間隔ステップ数

 &history  hst_file='$OUTPUTFILE', hst_intstep= 10, title='$TITLE' /

#------------------------ リスタート出力設定 --------------------------
#
# character(len=100) :: rst_file         ! リスタート出力ファイル名
# integer            :: rst_intstep      ! リスタート出力間隔ステップ数

 &restart  rst_file='$RSTFILE', rst_intstep=400 /

#--------------------------- 物理変数設定 ----------------------------
#
#  real(8) :: Radius    ! 惑星半径
#  real(8) :: Omega     ! 回転角速度
#  real(8) :: Alpha     ! 回転軸方向(極との角度,deg.)
#  real(8) :: Grav      ! 重力加速度
#  real(8) :: Hbar      ! 平均水深
#  integer :: HVOrder   ! 超粘性の次数
#                       ! (水平ラプラシアンの階数)
#  real(8) :: HVisc     ! 超粘性係数
#  integer :: HDOrder   ! 超拡散の次数
#                       ! (水平ラプラシアンの階数)
#  real(8) :: HDiff     ! 超拡散係数

 &physics  Radius=6.37122D6, Omega=7.292D-5, Alpha=90.0, 
           Grav=9.80616, Hbar = 3.0D3, 
           HVOrder=2, HVisc=0.5D16, HDOrder=2, HDiff=0.5D16       /

#---------------------------- 実験パラメター ----------------------------
#
# real(8) :: U0         ! 剛体回転流の速度(m/sec)

 &case2  U0 = 3.861068D1 /

######################################################################
END_OF_DATA

echo Program started at `date` > $LOGFILE
($TIMECOMMAND $PROGRAM) 1>> $LOGFILE 2>&1 || true
rm $NMLFILE
echo Program ended at `date` >> $LOGFILE

# 終了時に e-mail で通知する場合, 以下をコメントアウト
#$MAIL $EMAIL < $LOGFILE
