#!/usr/bin/env bash
#
# plbaro_lfim_test1.1.f90 用実行シェルスクリプト
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
# 履歴  2005/10/19 竹広真一 作成
#
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

TITLE="Test of linear terms of 2-dim barotropic model on a double-cyclic domain"


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
# logical :: Verbose    ! 冗長なメッセージ出力 On/Off
# logical :: DebugOn    ! デバッグメッセージ On/Off

 &message Verbose=.false., DebugOn=.false. / 

#---------------------------- 解像度設定 -----------------------------
#
#  integer :: km                            ! X 方向切断全波数
#  integer :: lm                            ! Y 方向切断全波数
#  integer :: im                            ! X 方向格子点数 (>3*km)
#  integer :: jm                            ! Y 方向格子点数 (>3*lm)

 &gridset km=21, lm=21, im=64, jm=64 /

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

 &tint    delta_t= 1.0d-004,  nstep= 100000       /

#------------------------------ 出力設定 ------------------------------
#
#  character(len=100) :: hst_file           ! ヒストリーファイル名
#  character(len=100) :: title              ! タイトル
#  integer            :: hst_intstep        ! ヒストリー出力間隔ステップ数

 &history  hst_file='$OUTPUTFILE', hst_intstep= 2000, title='$TITLE' /

#------------------------ リスタート出力設定 --------------------------
#
#  character(len=100) :: rst_file           ! リスタート出力ファイル名
#  integer            :: rst_intstep        ! リスタート出力間隔ステップ数

 &restart  rst_file='$RSTFILE', rst_intstep=1000 /

#------------------------ タイムフィルター設定 --------------------------
#
#  real(8) :: TFiltCoef    = 0.05         ! タイムフィルター係数
#  integer :: filt_intstep = 1            ! フィルターするステップ間隔

 &tfilter  TFiltCoef=0.05, filt_intstep=1 / 

#--------------------------- 物理変数設定 ----------------------------
#  real(8)            :: XLength            ! 領域の大きさ(X 方向)
#  real(8)            :: YLength            ! 領域の大きさ(Y 方向)
#  real(8)            :: Beta               ! βパラメター
#  integer            :: HVOrder            ! 超粘性の次数(1 で普通の粘性,
#                                           ! 水平ラプラシアンの階数)
#  real(8)            :: HVisc              ! 超粘性係数

 &physics XLength=1.0D0, YLength=1.0D0, Beta=1.0, HVOrder=2, HVisc=1.0D-6 /

#---------------------------- 実験パラメター ----------------------------
# 初期値 1 波数成分のみ
#
# integer :: KInitial             ! 波数 X 成分
# integer :: LInitial             ! 波数 Y 成分
# real(8) :: Amplitude            ! 振幅

 &initwave KInitial=3, LInitial=2, Amplitude=1.0D0 /

######################################################################
END_OF_DATA

echo Program started at `date` > $LOGFILE
($TIMECOMMAND $PROGRAM) 1>> $LOGFILE 2>&1 || true
rm $NMLFILE
echo Program ended at `date` >> $LOGFILE

# 終了時に e-mail で通知する場合, 以下をコメントアウト
#$MAIL $EMAIL < $LOGFILE
