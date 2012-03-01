#!/usr/bin/env bash
#
# chtbaro_freedecay_rk2cn-galerlin_exp1.f90 用実行シェルスクリプト
#
#   スクリプト名の形式 : ${ROOTNAME}-${SEED}-${NUMBER}.sh
#
#   - ${ROOTNAME} = スクリプトファイル名から
#                   パターン /-*([0-9])-*([0-9]).sh/ を除いた名前
#   - ${SEED}     = スクリプト名の 1 つめの番号部分. 
#                   ランダム初期値の種として使われる
#   - ${NUMBER}   = スクリプト名の 2 つめの番号部分. 
#                   リスタートファイルを使って継続計算するときの番号. 
#
#   - ${ROOTNAME}.out を実行しする. 
#
#   - INPUTFILE が空の場合にはプログラム内部に組み込まれた初期値が使われる. 
#
#   - ひきつづき時間発展を継続したい場合には, ${NUMBER} を 1 つ増やした
#     スクリプト名としてコピーし, initial_time を修正し, 実行すれば良い. 
#
#   - 異なる初期値からの計算(アンサンブルメンバー)をする場合には, 
#     ${SEED} の部分の番号を別の値にしたスクリプトにコピーして実行すれば良い.
#
#   - 終了時にメールでお知らせしたいときには EMAIL に通知する先のアドレス,
#     MAIL にメールコマンドを設定する. 
#
#
# 履歴  2005/10/31 竹広真一 作成
#       2007/11/30 竹広真一 コマンド引数を解析し, NAMELIST ファイル名を取得
#       2007/12/02 森川靖大 プログラム名, EMAIL 処理変更
#
#
set -e                          # エラー時スクリプトが止まる
shopt -s extglob                # 拡張パターンマッチング有効

TIMECOMMAND=time                # 時間計測コマンド


ROOTNAME=${0/-*([0-9])-*([0-9]).sh/}
NUMBER=${0/$ROOTNAME-/} ; SEED=${NUMBER/-*([0-9]).sh/} 
NUMBER=${NUMBER/$SEED-/} ; NUMBER=${NUMBER/.sh/}

PROGRAM=./${ROOTNAME}.out                    # メインプログラム
NMLFILE=${ROOTNAME}-${SEED}-${NUMBER}.nml  # NAMELIST ファイル
LOGFILE=${ROOTNAME}-${SEED}-${NUMBER}.log  # メッセージ出力ログファイル

INPUTFILE=''                               # 内部で初期条件設定
                                           # 初期条件リスタートファイル
INPUTFILE= #${ROOTNAME}-${SEED}-rst-$((${NUMBER}-1)).nc 
OUTPUTFILE=${ROOTNAME}-${SEED}-${NUMBER}.nc

OUTPUTFILE=${ROOTNAME}-${SEED}-${NUMBER}.nc          # 出力データファイル
RSTFILE=${ROOTNAME}-${SEED}-rst-${NUMBER}.nc         # リスタートデータファイル

# 実験タイトル

TITLE="Free-decay turebulence of 2-dim barotropic fluid on a channel domain"



# 終了時に e-mail で通知する場合, 以下の EMAIL に宛先の
# メールアドレスを指定し, コメントアウトを外す.

#EMAIL=hogehoge@gfd-dennou.org                 # メールアドレス
#EMAIL=${USER}@gfd-dennou.org                  # メールアドレス
MAIL="rsh silicon mailx -s '$TITLE'"           # 送信メールコマンド

if [ -e $NMLFILE ]; then
    rm $NMLFILE ;
    echo "Old $NMLFILE was removed." ; 
fi
echo $NMLFILE
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

# &gridset km=21, lm=21, im=64, jm=64 /
# &gridset km=42, lm=42, im=128, jm=128 /
# &gridset km=85, lm=85, im=256, jm=256 /
 &gridset km=85, lm=170, im=256, jm=512 /
# &gridset km=170, lm=170, im=512, jm=512 /

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

 &restart  rst_file='$RSTFILE', rst_intstep=1000000 /

#--------------------------- 物理変数設定 ----------------------------
#  real(8)            :: XLength            ! 領域の大きさ(X 方向)
#  real(8)            :: YLength            ! 領域の大きさ(Y 方向)
#  real(8)            :: Beta               ! βパラメター
#  real(8)            :: Visc=1.0D0         ! 粘性係数
#  character(len=2)   :: velBC              ! 流れ境界条件(RR/RS/SR/SS)

 &physics XLength=6.2831853D0, YLength=6.2831853D0, 
          Beta=100.0, Visc=2.0D-4, velBC='SS' /

#---------------------------- 実験パラメター ----------------------------
#  integer    :: Seed               ! seed(1)に設定する種の値
#  real(8)    :: Nmin               ! 初期エネルギー分布の全波数領域域最小値
#  real(8)    :: Nmax               ! 初期エネルギー分布の全波数領域最大値
#  real(8)    :: Etotal             ! 初期平均エネルギーの値

 &initvalue Seed=$SEED, Nmin=38.0D0, Nmax=40.0D0, Etotal=1.0D0 /

######################################################################
END_OF_DATA

echo Program started at `date` > $LOGFILE
($TIMECOMMAND $PROGRAM $NMLFILE) 1>> $LOGFILE 2>&1 || true
rm $NMLFILE
echo Program ended at `date` >> $LOGFILE

# 終了時に e-mail で通知する場合, 以下をコメントアウト
if [ -n "$EMAIL" ]; then
    $MAIL $EMAIL < $LOGFILE
fi
