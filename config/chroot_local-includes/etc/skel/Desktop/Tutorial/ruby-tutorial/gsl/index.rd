=begin

= Ruby/GSL

((:<font size="-1">:))
* 編集者: 塚原大輔, 中野満寿男
* 最終更新: 2006/03/08 (中野満寿男)
* 新規作成: 2005/02/12 (塚原大輔)
((:</font>:))

((:<hr>:))
((:<ol class="contents">:))
Contents
<<< index.hindex.rd
((:</ol>:))
((:<hr>:))

== Ruby/GSL とは

GSL(GNU Scientific Library) はその名の通り科学技術計算ライブラリで
さまざまな数値計算法の関数がたくさん集められています．((<URL:http://www.gnu.org/software/gsl/>))
ANSI C で記述されていて，C や C++ から呼び出せます．


Ruby/GSL は国立天文台の常定さんがメンテナンスしておられる
GSL の Ruby ラッパーです. 最新版(2006/03/08 現在) 1.7 では
GSL 1.7 の全ての関数を網羅しています. ((<URL:http://rb-gsl.rubyforge.org>)) 

* 利点
  * 無料で利用できる
  * C 版は日本人も多く利用(日本語ドキュメントが多数存在)
  * Ruby 版の一部のメソッドは引数, 戻り値に NArray をとることができる
    * インストール時に設定の必要あり
  * 描画ライブラリも付属
  
このチュートリアルでは Ruby/GSL の使用例をお見せします.   

== バイナリパッケージでインストール
*Debian, Vine

  # apt-get install rb-gsl

*Fedora Core

  # yum install rb-gsl

*FreeBSD

  # cd /usr/ports/math/ruby-gsl
  # make install

*Cygwin

ruby-gsl というパッケージが電脳Ruby プロジェクトから提供されています。詳しくは((<こちら|URL:http://dennou-k.gfd-dennou.org/arch/ruby/products/cygwin/index-j.html>))。

== ソースコードからインストール

=== plotutils のインストール

まずは gsl で使われている描画ライブラリをインストールします.
(rb-gsl に必須ではないらしいのですが, インストールでこけるので一応.)
((<Plotutils 公式ページ(GNU)|URL:ftp://ftp.gnu.org/gnu/plotutils/>))から

* plotutils-2.4.1.tar.gz

をダウンロードします.

展開した後に, 展開されたディレクトリに移ります. そして

  % ./configure

で Makefile を生成します. この時デフォルトのインストールパス(/usr/local/)
以外の場所にインストールしたい場合は

  % ./configure --prefix=<インストール先のパス>

などとしましょう. configure が終わったら
  
  % make

します. 時間がかかるので紅茶でも飲んで待ちましょう. 終わったら
  
  # make install

します(場合によってスーパーユーザになってください). 簡単ですね. 

=== GSL のインストール

((<GSL 公式ページ(GNU)|URL:ftp://ftp.gnu.org/gnu/gsl/>))から

* gsl-1.7.tar.gz

をダウンロードしましょう.

展開した後に, 展開されたディレクトリに移ります. そして plotutils 同様に

  % ./configure or ./configure --prefix=<インストール先のパス>
  % make
  # make install

とします. これで終了です.

=== rb-gsl のインストール

次に Ruby/GSL をインストールします.
((<RubyGSL 公式ページ|URL:http://rb-gsl.rubyforge.org/>))の 3. Installation, 2 の Download から
((<URL:http://rubyforge.org/frs/?group_id=285>))に飛びます. ここから

* rb-gsl-1.7.0.tar.gz

をダウンロードしましょう.

落とした資源を展開して, 展開したディレクトリに移ります.

まずは設定します. NArray を gsl で利用できるようにするには

  % ruby setup.rb config -- --with-narray-include=<narray.h's path>

とします. この時 narray.h のパスを陽に指定しなくてはなりません.
また plotutils や gsl のインストール先がデフォルトのロードパスにない場合は
以下の様に設定してやらなければなりません.

  % export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:<インストール先のパス>/lib
  % export C_INCLUDE_PATH=$C_INCLUDE_PATH:<インストール先のパス>/include

実行した際に, narray.h がロードされたことを確認したら

  % ruby setup.rb setup

とします. 無事終了したら  
  
  # ruby setup.rb install

します. ただし, rb_gsl.so が取り残されるかもしれません. その時は手動で

  # cd ext; install rb_gsl.so <インストール先のパス>

と実行して移してやります. 以上でインストールは終了です.

それではサンプルを実行してみましょう. サンプルは展開したディレクトリの
samples/ 以下にあります. ((<リファレンスマニュアル|URL:http://rb-gsl.rubyforge.org/ref.html>))
を眺めつつお好みのプログラムを実行してみましょう.

== 使用例


=== EOF 解析
Ruby/SSL2 の例題スクリプトを参考にしています. ただし欠損値は考慮してません.

((<eof_gsl.rb|URL:eof_gsl.rb>)): GSL::Eigen::symmv(実対称行列の固有値固有ベクトルを求める) の利用
=end
=begin html
<textarea cols="80" rows="10" wrap="hard" class="source">
require "narray"
require "gsl"
 
module Analysis
 
def covariance(x,y)
  if x.length != y.length then
    print "error\n"
    exi
  end

  len = x.length

  sum = x.mul_add(y,0)
  n = len

  return  sum/(n-1)
end

def covariance_matrix( x )
  if x.rank !=2
    raise "covariance_matrix: x.rank must be 2"
  end

  dim = x.shape[0]  
  cov = ( NArray.sfloat(dim, dim).fill!(0.0) ).to_gm
  total = 0

  for i in 0..dim-1
    for j in 0..i
      elm = covariance(x[i,true],x[j,true])
      cov[j, i] = elm  unless i == j
      cov[i, j] = elm 
      total += elm
    end
  end
  return cov, total
end

def eof( x )

  if x.shape.length!=2 then
    print "err\n"
    exit
  end

  dim,nle = x.shape

  p "calc anomary"
  x = x - x.mean(1)

  p "make covariance matrix"
  cova, total = covariance_matrix(x)

  p "calc eigen value"
  val, vec = GSL::Eigen::symmv(cova)
  vec = vec.transpose.to_na

  [val/total,vec]
end

module_function :eof, :covariance_matrix, :covariance

end
</textarea>
=end html
=begin


((<eof.rb|URL:eof.rb>))eof_gsl.rb で定義した Analysis::eof を呼ぶスクリプト.
=end
=begin html
<textarea cols="80" rows="10" wrap="hard" class="source">
require "narray"
require "gsl"
require "eof_gsl.rb"
require "numru/dcl"

include NumRu

def rand_nmal(m, s, n)

  # 正規乱数作成
  rnd_std_nmal = NArray.sfloat(n).randomn 
  rnd_nmal     = rnd_std_nmal * s + m 

  return rnd_nmal
end

n = 1000
x = NArray.float(2,n)

a = rand_nmal(0,0.4,n)  # 平均 0, 分散 0.4 の正規乱数
b = rand_nmal(0,0.2,n)  # 平均 0, 分散 0.2 の正規乱数

theta = Math::PI/6
x[0,true] = a*Math::cos(theta) - b*Math::sin(theta)
x[1,true] = a*Math::sin(theta) + b*Math::cos(theta)

val,vec = Analysis.eof(x)

DCL::gropn(1)
DCL::grfrm
DCL::grswnd(-1.5,1.5,-1.5,1.5)
DCL::uspfit
DCL::grstrf
DCL::usdaxs
DCL::sgpmzu(x[0, true],x[1,true],1,1,0.01)
for i in 0..1
  fact = val[i]
  DCL::sgplzu([-vec[0,i]*fact,vec[0,i]*fact],[-vec[1,i]*fact,vec[1,i]*fact],1,3)
end
DCL::grcls
</textarea>
=end html
=begin

eof.rb の実行結果

((:<center><IMG SRC="eof.png"></center>:))
    
=end


