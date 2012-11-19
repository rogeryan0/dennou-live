#!/usr/bin/env bash
#
# NS_rot-3d-shell_abcn_restart_namelist_rigidrot.f90 用実行シェルスクリプト
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
# 履歴  2005/02/11 竹広 真一 作成
#       2005/02/19 竹広 真一 剛体回転初期値用に改造
#
#
shopt -s extglob                # 拡張パターンマッチング有効

TIMECOMMAND=time                # 時間計測コマンド

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
