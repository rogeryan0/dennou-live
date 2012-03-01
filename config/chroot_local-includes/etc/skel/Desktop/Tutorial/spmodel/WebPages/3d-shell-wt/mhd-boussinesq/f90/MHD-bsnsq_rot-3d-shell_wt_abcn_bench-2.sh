#!/usr/bin/env bash
#
# 表題  回転球殻ブシネスクダイナモモデル
#
#       -- ベンチマーク実行用シェルスクリプト
#
#   スクリプト名の形式 : ${ROOTNAME}-${NUMBER}.sh
#
#   - ${ROOTNAME} = スクリプトファイル名から
#                   パターン /-*([0-9])-*([0-9]).sh/ を除いた名前
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
#   - 終了時にメールでお知らせしたいときには EMAIL に通知する先のアドレス,
#     MAIL にメールコマンドを設定する. 
#
# 履歴  2007/09/13  竹広真一    コメント増強
#       2007/12/20  竹広真一    コメント増強
#
#
shopt -s extglob                # 拡張パターンマッチング有効

#TIMECOMMAND=timex               # 時間計測コマンド
TIMECOMMAND=/usr/bin/time               # 時間計測コマンド

ROOTNAME=${0/-*([0-9]).sh/}
NUMBER=${0/$ROOTNAME-/} ; NUMBER=${NUMBER/.sh}

PROGRAM=${ROOTNAME}.out
NMLFILE=${ROOTNAME}-${NUMBER}.nml  # NAMELIST ファイル
LOGFILE=${ROOTNAME}-${NUMBER}.log
INPUTFILE=${ROOTNAME}-$((${NUMBER}-1)).nc
OUTPUTFILE=${ROOTNAME}-${NUMBER}.nc

# 実験タイトル

TITLE="dynamo benchmark (case1) (T42L32)"

# 終了時に e-mail で通知する場合, 以下の EMAIL に宛先の
# メールアドレスとメール送信コマンドを指定しコメントを外す.
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
# メッセージコントロール
#    verbose = .ture. で namelist の入力を促すメッセージが出力される.
#
 &message verbose=.false., DebugOn=.false. /
# &message verbose=.false., DebugOn=.true. /

#
# 解像度設定
#    nm:水平最大全波数, lm:鉛直チェビシェフ最大次数
#    im: 経度方向格子点数, jm: 緯度方向格子点数, km: 動径方向格子点数
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
# 球殻内外半径設定
#   eta : 球殻内外半径比, ri : 内側球殻半径, ro: 外側球殻半径
#   eta のみ与えると自動的に ro-ri=1 となるように ro, ri が設定される. 
#
 &radius  eta=0.35 /

#
# 初期値設定
#    initial_file : 初期値ファイル. 空白ならば内部で初期値が与えられる
#                   (静止状態, 球殻真中に 1 点擾乱温度場)
#    initial_time : 初期値の時間
#
 &initial initial_file='$INPUTFILE', initial_time= 1.0   /

#
#  時間積分設定
#       delta_t : 時間刻み
#       nstep   : 総計算ステップ数
#
 &tint    delta_t=1.0d-004,  nstep= 100000    /

#
# 出力設定
#    output_file : 出力ファイル名
#    ndisp       : 出力時間ステップ間隔
#    title       : ファイルに書き込まれる実験名
#
 &output  output_file='$OUTPUTFILE', NDISP= 5000, title='$TITLE' /

#
# 物理パラメター設定
#    Ra     : レイリー数
#    Pr     : プランドル数
#    Ta     : テイラー数
#    Pm     : 磁気プランドル数
#
 &physics Ra=100.0d0, Pr=1.0d0, Ekman=1.0d-3, Pm=5.0d0 /

#
# 境界条件設定
#     VelBC  : 速度境界条件
#              FF : 両端自由すべり
#              FR : 上端自由すべり, 下端粘着
#              RF : 上端粘着, 下端自由すべり
#              RR : 両端粘着
#
#     TempBC : 温度擾乱境界条件(値は 0 に設定される)
#              DD : 両端温度固定
#              DN : 上端温度固定, 下端温度傾度固定
#              ND : 上端温度傾度固定, 下端温度固定
#              NN : 両端温度傾度固定
#
 &boundary VelBC='RR', TempBC='DD', 
           Temptop=0.0d0, Tempbottom=1.0D0 /
#           Temptop=0.0d0, Tempbottom=-1.66666666666666666666 /
#                             dT/dr = -1/eta at r=ri

#
# 初期値の種類
#      case=0 : 磁場なし実験
#      case=1 : 磁場あり実験
#
 &inittype case=1 / 

######################################################################
END_OF_DATA

echo Program started at `date` on `hostname` > $LOGFILE
echo $TIMECOMMAND $PROGRAM $NMLFILE
($TIMECOMMAND $PROGRAM $NMLFILE) 1>> $LOGFILE 2>&1 || true
rm $NMLFILE
echo Program ended at `date` >> $LOGFILE

# 終了時に e-mail で通知する. 
if [ -n "$EMAIL" ]; then
    $MAIL $EMAIL < $LOGFILE
fi
