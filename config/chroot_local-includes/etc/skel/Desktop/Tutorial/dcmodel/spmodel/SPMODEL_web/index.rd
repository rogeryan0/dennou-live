=begin JA
= 階層的地球流体スペクトルモデル集 SPMODEL
=end JA
=begin EN
= SPMODEL: Hierarchical Spectral Models for GFD
=end EN

=begin

# * 2010/03/02 (佐々木洋平)
# * 2008/03/04 (竹広真一) 
# * 2008/02/19 (竹広真一) 
# * 2008/01/12 (竹広真一) 
# * 2008/04/23 (佐々木洋平) 
# * 2007/12/05 (佐々木洋平) 
# * 2007/10/10 (佐々木洋平)
# * 2005/12/14 (林祥介)
# * 2005/10/01 (竹広真一) 
# * 2005/07/25 (小高正嗣) 
# * 2005/07/21 (小高正嗣) 
# * 2005/06/30 (小高正嗣) 
# * 2005/04/01 (小高正嗣) 
# * 2005/04/01 (小高正嗣)
# * 2005/03/30 (小高正嗣)
# * 2004/03/18 (小高正嗣)
# * 2004/01/26 (小高正嗣)
# * 2002/09/12 (竹広真一, 林祥介)
# * 2002/02/15 (竹広真一) 新規作成

=end

=begin JA 

地球流体電脳倶楽部スペクトルモデルプロジェクトでは, 地球流体力学の研究
や教育に使えるスペクトル法を用いたさまざまな数値モデルをそろえる活動を
進めています.


=end JA
=begin EN

In SPMODEL project, we are now developing hierarchical spectral models
for geophysical fluid dynamics which can be used for research and
education with ease.  


=end EN

=begin HTML
<table align="right">
<tr><td>
<A href="gallery/shallow-topo/">
<IMG ALT="shallow_topo_linear_small.jpg" SRC="./gallery/shallow_topo_linear_small.jpg" width=240, BORDER=1></a>
</td>
<td>
<A href="gallery/kh-instability//">
<IMG ALT="kh1.gif" SRC="./gallery/kh-instability/kh1.gif" align="center" BORDER=1 width=240></a>
</td>
</tr>
</table>

=end

=begin JA

* ((<SPMODEL の目指すもの>))
* ((<ギャラリー|URL:./gallery/index.htm>))
* ((<SPMODEL の使い方|URL:./html/usage.htm>))
  * ((<SPMODEL に必要となる資源|URL:./html/needed.htm>))
  * ((<SPMODEL ライブラリ (spml)|URL:./html/spml.htm>))
* ((<プログラム集|URL:./html/sample.htm>))
* ((<チュートリアル資料|URL:./tutorial/index.htm>))
* ((<Live CD|URL:./LiveCD/index.htm>))
* ((<使用上の注意|URL:./html/licence.htm>))
* ((<発表資料|URL:./html/paper.htm>))
* ((<謝辞|URL:./html/acknowledge.htm>))
* ((<リンク・開発者向け資料|URL:./html/link.htm>))

== ニュース(2010.03.02)

SPMODEL ライブラリ(spml) の新規バージョン 0.6.0 をリリースしました.
今回の更新/変更点は以下の通りです.

* 新規追加項目
  * x 方向周期境界, y 方向無限領域のモジュール(ef_module)
  * 3 次元水路領域のモジュール (tee_module)

== SPMODEL の目指すもの

SPMODEL では, 地球流体力学に登場するさまざまなレベルの近似方程式系の数
値モデルを, 空間 1 次元モデルから 2 次元あるいは 3 次元モデルまで階層
的に整備しています.

このような一連の数値モデルをそろえることで
* 標準的あるいは重要な GFD のイラストレーションをたやすく再現できるようにしたい
  → 地球流体力学の理解と普及に役立てる
* 数値計算による知見を研究者の間で共有したい
  → 従来の数式による理解から数値計算による理解へ
* 一連のモデルの振舞の比較をスムーズに行いたい
  → より複雑なモデルの結果を理解するための道具
といったことを目指しています. 

このため, 現在整備しようとしているモデルシリーズの基本方針では「可読性
が高く理解しやすく, 変形しやすいこと」を最重点においています. 大名プロ
グラマー主義(CPUは無限に速く, メモリとディスク容量は無限に大きいという
環境の下で仕事をする) にしたがって, スピードはとりあえず犠牲にしても構
わない, というスタンスです.  しかしながらこのモデルシリーズをベースに
することで, 例えば最先端の大計算を行うに耐える高速なモデルの開発などが
容易になることも期待しています.

SPMODEL のモデルでは, 格子点とスペクトル空間のデータ変換や空間微分などの
基本的な配列関数からなる SPMODEL library (spml) を用いてます. 
Fortran90 の配列機能を生かしたこのライブラリの配列関数を用いることで, 
時間発展方程式の時間変化項以外の部分を
数式の形そのままにプログラミングすることができるようになってます. 
支配方程式の形をそのままプログラムソースに反映させられるので
プログラムの可読性を向上させることができます. 
また, spml の配列関数は入出力配列の性質が名前からわかるようにするべく
統一的に命名法にしたがっているので, 関数の使い方が機械的になり, 
プログラムを修正することも容易に行えるようになっています. 

SPMODEL のライブラリとサンプルプログラム自体が Fortran90 のプログラミ
ング書法の一つの実験でもあります. 地球流体力学の問題に限らずさまざまな
物理現象のスペクトル法による数値計算の一つのスタイルとして参考にして頂
ければと思います.


=end JA
=begin EN

* ((<The goal of SPMODEL>))
* ((<Gallery|URL:./gallery/index.htm.en>))
* ((<How to use SPMODEL ?|URL:./html/usage.htm.en>))
  * ((<Required softwares|URL:./html/needed.htm.en>))
  * ((<SPMODEL library (spml)|URL:./html/spml.htm.en>))
* ((<SPMODEL programs|URL:./html/sample.htm.en>))
* ((<Tutorial|URL:./tutorial/index.htm>))
* ((<Live CD|URL:./LiveCD/index.htm>))
* ((<Licence|URL:./html/licence.htm.en>))
* ((<Publication|URL:./html/paper.htm.en>))
* ((<Acknowledgement|URL:./html/acknowledge.htm.en>))
* ((<Link & Developer's archive|URL:./html/link.htm.en>))

== News(2010.03.02)

New version (0.6.0) of SPMODEL library (spml) was released. 
Modified points and newly implemented functions are as folllows:

* Newly implemented functions
  * Functions for mpi calculation (which call MPI routines of ISPACK)
  * A module for disk and sphere geometry by using Jacobi polynomials (aq_module)
  * A module for disk and sphere geometry by using Chebyshev polynomials and Gauss-Radau grid (au_module)
  * A module for axisymmetric geometry by using Legendre polynomials(l_module)
  * Modules for converting models of sphere and sperical shell geometries
    to the axisymmetric version (w_zonal_module, wa_zonal_module, wt_zonal_module)

* Modified points
  * Declaration of grid data arrays was changed from (im,jm) to (0:im-1, 1:jm).
  * Almost documantation were produced by rdoc-f95. 

In particular, please take care of the declaration of grid data arrays in
the spherical harmonics modules when you upgrade the spml library from 
the older version. 

== The goal of SPMODEL

In SPMODEL project, we develop numerical models based on various
approximation equation systems that appear in the geophysical fluid
dynamics hierarchically.

=end EN

