#! ruby -Ks
# -*- coding: japanese-cp932 -*-
# [注意]
# サンプルスクリプト内に日本語を使用する場合、
# スクリプトの最初に上記の記述を行わないと
# 文字化けの要因となる。
###########################################################################
#
# -*- coding: cp932 -*-
#
###########################################################################
# = knowledge_sample_creating_document.rb
#   Gfdnavi の Webサービスを用いて知見文書を作成するプログラム。
#   * Gfdnaviインストール時に含まれるサンプルデータを用いて
#     画像を作成し、その画像を含めた知見文書を保存する
#   * 簡単な描画オプションを幾つか指定して作図を行い、
#     その図を含めた文書を作成するサンプル。
###########################################################################
# == 実行に必要な環境
#    * Ruby 1.8
#    * gfdnavi_data
#      http://www.gfd-dennou.org/arch/davis/gfdnavi/download/
#      より取得できる。
#      $ ruby install.rb
#      としてインストールする。
###########################################################################

# == はじめに
require "numru/gfdnavi_data"
require "pp"
include NumRu

# === ヘルプを出力できるように
if ARGV.include?("-h") || ARGV.include?("--help")
  print "USAGE: ruby #$0 [OPTIONS] [gfdnavi_webservice_portal_URL]\n"
  print "OPTIONS: \n"
  print "   -u [user_name] : If this option is none, interpreted as \"root\".\n"
  print "   -h             : Display this help.\n"
  print "gfdnavi_webservice_portal_URL: \n"
  print "   ex) http://0.0.0.0:3000/data        <- default \n"
  exit
end

# === このサンプルプログラムを用いて、root 以外のユーザとして
#     ドキュメントを作成するときにはオプションでアカウント名を指定する
#     (指定するアカウントが、接続する Gfdnavi 上に作成されている必要がある)
if (u_option = ARGV.index("-u"))
  user = ARGV[u_option +1]
  ARGV.delete_at u_option
  ARGV.delete_at u_option
else
  user = "root"
end

# === URL の処理
url_prefix = ARGV.shift || "http://0.0.0.0:3000/data"
url_prefix = url_prefix.sub(/\/\z/, "")

# === 画像を imagemagick を利用して表示するためのメソッド
def display(plot)
  IO.popen("display","w"){|io| io.print(plot.to_png)}
end

###########################################################################
# == ここから、文書作成のサンプル
###########################################################################

# === 知見文書を作成する
# 初めに、GfdnaviData::KnowledgeRemote オブジェクトを作成する必要がある
nk = GfdnaviData::KnowledgeRemote.new
# 文書を保存するパスを指定する(必須)
nk.url         = url_prefix + "/usr/#{user}/knowledge/knowledge_sample_creating_document.knlge"
# タイトルを指定する(必須)
nk.title       = "Temperature data from ncep!!!"
# 作成者を指定する(必須)
nk.user        = user
# 文書のカテゴリを指定する(必須)
nk.category    = "memo"
# 表示される「著者名」を指定する
nk.creator     = "Davis Taro"
# 文書の要約を指定する
nk.description = "Australia is warm."
# 本文の内容を記述する
#nk.textbody    = "The most heated area in Figure 1 is a point of 130 degrees of east longitude, 20 degrees of south latitude.\nThat is, it's Australia."
nk.textbody    = "ncepのデータを元に((<figure 1>))を作成した。\n
これは全球の地表面の温度をプロットしたものである。\n
この図を見てみると、最も気温の高い、ピンク色に表示された部分があることが分かる。\n
そこで、地図投影を行い、どのような地域であるかを調べてみることにした(((<figure 2>)))。\n
少々見づらいが、この高温の地域はオーストラリアに含まれていることが分かる。\n
また、((<figure 3>))にこの地域を拡大して表示してみた。\n
中心部は32度を超えることが分かった。\n
\n\n
((<figure 4>))は北極点を中心とした地図投影である。\n
この図を見ると、極から赤道に向かって、緯度が低くなるに従い気温が低くなっていく様子がよく分かる。\n
"

# === 文書に入れたい図を作成する。
# まず、GfdnaviData.open する必要がある
t = GfdnaviData.open(url_prefix + "/samples/reanalysis/ncep/T.jan.nc/T")

# 得られた GfdnaviData::VariableRemote オブジェクトに対して
# plot メソッドを使うことで、描画される。
image01 = t.plot("tone_contour")
# オプションの指定には、ハッシュを用いる。デフォルト値についてはこのファイルの最後に示す。
# (オプションの詳細は http://ruby.gfd-dennou.org/products/gphys/doc/ggraph.html を参照)

# 地図投影(メルカトル図法)
image02 = t.plot("tone_contour", {"projection"=>11})

# 表示範囲を限定してみる
opts = {
  "axes"=>
  {"lon"=>{"max"=>180, "min"=>100},
    "level"=>{"min"=>"1000"},
    "lat"=>{"max"=>"0", "min"=>"-45"}}, # 軸の指定を行う(範囲の指定も併せて行う)
  "x_axis"=>"lon",  
  "y_axis"=>"lat",  
  "z_axis"=>"level"
}
image03 = t.plot("tone_contour", opts)
#display image03
# ,
#  "color_bar"=>true # カラーバーの表示


# polar steleo projection.
opts = {
  "projection"=>31
}
image04 = t.plot("tone_contour", opts)

# === 作成した図を文書に加える。
#     add_figure= メソッドは、文書に図を1つ追加する。
#     (引数は plot の結果と、キャプション)
nk.add_figure(image01, "地表面の温度")
nk.add_figure(image02, "figure 1を地図に投影した(メルカトル図法)")
#nk.add_figure(image03, "高温部のみ大きく表示した") ### なんかimage03 だけオプションのせいかエラーが出る。要検証。
nk.add_figure(image04, "北極を中心とした地図投影")


# === 文書内の図の(デフォルトの)レイアウトを設定する
#     0: "layout_figures_under_text"
#     1: "layout_one_figure_above_text"
#     2: "layout_figures_in_a_row_above_text"
#     デフォルトは 0.
nk.default_layout = 0

# レイアウトが "layout_figures_under_text"のときのみ、
# 「横に何枚並べるか」を指定する。(デフォルトは1枚)
# (指定した枚数を越えた分は次の行に並ぶ)
nk.horizontal_figures = 2


# === 図の大きさをまとめて指定する。
# (1) 「高さか、幅のどちらを指定するか」を指定する。
#     0: height 
#     1: width
#     デフォルトは 0.
nk.figures_size_height_or_width = 0

# (2) 「ピクセル単位での指定か、元々の大きさに対する倍率での指定か」を指定する。
#     0: %
#     1: px
#     デフォルトは 0.
nk.figures_size_units = 0

# (3) 数値を指定する。(1), (2) と併せて解釈され、表示される図の大きさが決まる。
#     デフォルトは 100.
nk.figures_size_number = 120


# === 保存する。save メソッドを用いる。
if nk.save
  print "Save is successful.\n"
end
exit

# 描画オプションのデフォルト値
=begin
opts = {"coloring"=>false, # true/false
  "color_bar"=>false,      # true/false
  "log"=>false,            # true/false
  "exchange"=>false,       # true/false
  "variables_order"=>"", # ?
  "projection"=>1,       # 数値
  "map_fit"=>true,        # true/false
  "contour"=>true,        # true/false
  "tone"=>true,           # true/false
  "axes"=>
  {"lon"=>{"max"=>"350", "min"=>"0"},
    "level"=>{"min"=>"1000"},
    "lat"=>{"max"=>"-90", "min"=>"90"}},
  "x_axis"=>"lon",       # 文字列
  "y_axis"=>"lat",       # 文字列
  "z_axis"=>"level",     # 文字列
  "viewport"=>["0.2,0.8,0.2,0.8"],              # 今のところは文字列(配列にするとURLに半角スペースが混じる)
  "map_window"=>["-180.0,180.0,-75.0,75.0"]}    # 今のところは文字列
=end

#  "viewport"=>[0.2,0.8,0.2,0.8],
#  "map_window"=>[-180.0,180.0,-75.0,75.0]}

# exchange : true or 1 or "1" なら true 判定
# map_fit  : うまく働いているのかどうかよく分からない
# viewport : X軸、Y軸それぞれの表示範囲を設定する。


