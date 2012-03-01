require "gphys_cont2"
#
#= GPhys を利用して色塗り図を描画するクラスライブラリ
#
#Authors::   森川 靖大
#Version::   1.2 2006-03-08 morikawa
#Copyright:: Copyright (C) GFD Dennou Club, 2006. All rights reserved.
#License::   Ruby ライセンスに準拠
#
#-- (#-- から #++ までの部分を RDoc は解釈しません.)
#"=", "==", ""===" は見出しを表します.
#
#= 見出しレベル1
#== 見出しレベル2
#=== 見出しレベル3
#++
#
#このクラスのスーパークラスは GPhys_cont です.
#new メソッドで初期化を行い, tone メソッドで描画を行います.
#
#--
# モジュール名やメソッド名はそのままモジュールやメソッドへのリンクに
# 変換されます.
#++
#
#=== 参考資料
#
#* http://ruby.gfd-dennou.org
#  1. GPhys[http://www.gfd-dennou.org/library/ruby/products/gphys/]
#  2. {2006 年 電脳rubyセミナー・電脳davis/rubyワークショップ}[http://www.gfd-dennou.org/library/ruby/workshop200603/]
#
#--
#==リストの表示に関して
#
#リストは以下のような記号が付いたパラグラフです.
#
# - '*' もしくは '-' で普通のリスト
# - 数字+ピリオドで番号付きリスト
# - アルファベット+ピリオドでアルファベットリスト
#
#
#== リンクに関して
#
# http:, mailto:, ftp:, www. で始まるテキストはウェブへのリンクだと
# 判別されます.
#
# label[url] の形式でもハイパーリンクが張れます. この場合は lavel が表
# 示され, url がリンク先となります. label が複数の単語を含んでいる場合 
# (日本語の場合はこっちを使ってください), 中括弧を使い, <em>{multi word
# label}[</em>url<em>]</em>としてください.
#++
#
#=== 開発履歴
#
#* 1.2 2006-03-08
#  * 堀之内さんのコメントを下に, 作者やライセンス, 開発履歴の
#    欄を足してみる.
#
#* 1.1 2006-03-07
#  * とりあえず作成してみる.
#
class GPhys_tone < GPhys_cont

  # 図に色塗りを行うかどうかのフラグ.
  # このフラグを false や nil にした場合, GPhys_cont#cont と
  # 同様に動作します.
  #
  attr_accessor :draw_tone

  #
  #=== 初期化処理用メソッド
  #
  #GPhys_cont#new を参照してください.
  #
  #--
  # 別のモジュール内のメソッドへリンクする場合は
  # "<i>モジュール名</i>#<i>メソッド名</i>" と指定します
  #++
  #
  def initialize
    super
    @draw_tone = true
  end

  #=== 描画メソッド
  #
  #色塗り図を描画するメソッド. 等値線図のみを描画したい場合は
  #GPhys_cont#cont を利用してください.
  #
  #_itr_ :: 描画する際の地図投影法を指定します. 数値を与えてください.
  #         デフォルトは 1 になっています. 番号と投影法の関係に関しては
  #         http://www.gfd-dennou.org/library/dcl/dcl-f90/doc/term/2d.htm
  #         を参照ください
  #
  #返り値:: 常に true が返ります.
  #
  def tone(itr=1)
    gphys = GPhys::IO.open(FILENAME, @var)
    DCL.gropn(1) ; DCL.sgpset('lcntl', false) ; DCL.uzfact(0.7)
    GGraph.set_fig( 'itr'=>(itr == nil) ? 1 : itr.to_i)
    GGraph.tone( gphys ) if @draw_tone
    GGraph.contour( gphys, !@draw_tone )
    DCL.grcls
    return true
  end
end

if __FILE__ == $0
  gphys = GPhys_tone.new
  gphys.tone
end
