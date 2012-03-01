=begin JA
= 階層的地球流体スペクトルモデル集 SPMODEL
=end JA
=begin EN
= SPMODEL: Hierarchical Spectral Models for Geophysical Fluid Dynamics
=end EN

=begin

# * 2012/02/29 (佐々木洋平)
# * 2012/02/27 (佐々木洋平)
# * 2012/02/25 (佐々木洋平)
# * 2011/09/16 (佐々木洋平)
# * 2010/04/27 (佐々木洋平)
# * 2010/04/11 (竹広真一)
# * 2010/04/06 (佐々木洋平, 竹広真一)
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

地球流体電脳倶楽部スペクトルモデルプロジェクトでは,
地球流体力学の研究や教育に使えるスペクトル法を用いたさまざまな数値モデルをそろえる活動を進めています.


=end JA
=begin EN

SPMODEL project is now developing various hierarchical spectral models
for geophysical fluid dynamics which can be easily used for research
and education.

=end EN

=begin HTML
<table align="right">
<tr><td>
<A href="gallery/shallow-topo/">
<IMG ALT="shallow_topo_linear_small.jpg" SRC="./gallery/shallow_topo_linear_small.jpg" width=240, BORDER=1></a>
</td>
<td>
<A href="gallery/kh-instability/">
<IMG ALT="kh1.gif" SRC="./gallery/kh-instability/kh1.gif" align="center" BORDER=1 width=240></a>
</td>
</tr>
</table>

=end

=begin JA

# * ((<SPMODEL の目指すもの|URL:./html/goal.htm>))
* ((<SPMODEL の目指すもの>))
* ((<ギャラリー|URL:./gallery/index.htm>))
* ((<SPMODEL の使い方|URL:./html/usage.htm>))
  * ((<SPMODEL に必要となる資源|URL:./html/needed.htm>))
  * ((<SPMODEL ライブラリ (spml)|URL:./html/spml.htm>))
* ((<プログラム集|URL:./html/sample.htm>))
* ((<チュートリアル資料|URL:./tutorial/index.htm>))
* ((<使用上の注意|URL:./html/licence.htm>))
* ((<発表資料|URL:./html/paper.htm>))
* ((<開発メンバー|URL:./html/member.htm>))
* ((<謝辞|URL:./html/acknowledge.htm>))
* ((<リンク・開発者向け資料|URL:./html/link.htm>))

=end JA
=begin EN

# * ((<The goal of SPMODEL|URL:./html/goal.htm.en>))
* ((<The goal of SPMODEL>))
* ((<Gallery|URL:./gallery/index.htm.en>))
* ((<How to use SPMODEL ?|URL:./html/usage.htm.en>))
  * ((<Required softwares|URL:./html/needed.htm.en>))
  * ((<SPMODEL library (spml)|URL:./html/spml.htm.en>))
* ((<SPMODEL programs|URL:./html/sample.htm.en>))
* ((<Tutorial|URL:./tutorial/index.htm.en>))
* ((<Licence|URL:./html/licence.htm.en>))
* ((<Publication|URL:./html/paper.htm.en>))
* ((<Development group|URL:./html/member.htm.en>))
* ((<Acknowledgement|URL:./html/acknowledge.htm.en>))
* ((<Link & Developer's archive|URL:./html/link.htm.en>))
=end EN

=begin JA

== ニュース(2012.02.29)

SPMODEL ライブラリ(spml) の新規バージョン 0.7.1 をリリースしました.
今回の更新/変更点は以下の通りです.

* ライセンス変更
  * 独自オリジナル → MIT ライセンス.
    詳細は((<使用上の注意|URL:./html/licence.htm>))を参照下さい.
* 新規追加項目
  * 球面および球殻領域を解くためのモジュールの sjpack_cuda 使用版
    * w_module_sjpack_cuda, wa_module_sjpack_cuda, wt_module_sjpack_cuda
  * 三次元球および球殻用モジュールの MPI 並列化版
    * 三次元球: wq_mpi_module
    * 三次元球殻 + 内球: wtq_mpi_module
  * 三次元水路領域を解くためのモジュールの fftj 使用版
    * tee_module_fftj, eea_module, eea_module_fftj
  * 線形方程式を LU 分解で解く際に CUDA を使用するモジュール: lumatrix_cuda
    * 要 PGI Compiler + Accelerator (>= 2010)
* バグ修正/その他の変更など
  * 軸対称モジュールの内部作業領域の修正
  * FORTRAN77 のサブルーチンをすべて fortran 90 化

=end JA
=begin EN

== News(2012.02.29)

New version (0.7.1) of SPMODEL library (spml) was released.
Modified points and newly implemented functions are as follows:

* License changed
  * original license → MIT License.
    Please see ((<License|URL:./html/licence.htm.en>)) in detail.
* Newly implemented functions
  * Modules of sphere and spherical shell geometry using SJPACK_CUDA
    * w_module_sjpack_cuda, wa_module_sjpack_cuda, wt_module_sjpack_cuda
  * Modules of sphere and spherical shell geometry, MPI parallelization
    * wq_mpi_module, wtq_mpi_module
  * A module for solving systems of linear equations by use of CUDA: lumatrix_cuda
    * Depends: PGI Compiler + Accelerator (>= 2010)
* Bug fixed/misc. update
  * Modules of axial symmetry: fix working space size
  * Remove FORTRAN77 files: rewrite in fortran 90

=end EN

=begin JA
== SPMODEL の目指すもの

SPMODEL では, 地球流体力学に登場するさまざまなレベルの近似方程式系の数値モデルを, 空間 1 次元モデルから 2 次元あるいは 3 次元モデルまで階層的に整備しています.

このような一連の数値モデルをそろえることで
* 標準的あるいは重要な GFD のイラストレーションをたやすく再現できるようにしたい
  → 地球流体力学の理解と普及に役立てる
* 数値計算による知見を研究者の間で共有したい
  → 従来の数式による理解から数値計算による理解へ
* 一連のモデルの振舞の比較をスムーズに行いたい
  → より複雑なモデルの結果を理解するための道具
といったことを目指しています.

このため, 現在整備しようとしているモデルシリーズの基本方針では「可読性が高く理解しやすく, 変形しやすいこと」を最重点においています. 大名プログラマー主義(CPUは無限に速く, メモリとディスク容量は無限に大きいという環境の下で仕事をする) にしたがって, スピードはとりあえず犠牲にしても構わない, というスタンスです.  しかしながらこのモデルシリーズをベースにすることで, 例えば最先端の大計算を行うに耐える高速なモデルの開発などが容易になることも期待しています.

SPMODEL のモデルでは, 格子点とスペクトル空間のデータ変換や空間微分などの基本的な配列関数からなる SPMODEL library (spml) を用いてます. Fortran90 の配列機能を生かしたこのライブラリの配列関数を用いることで, 時間発展方程式の時間変化項以外の部分を数式の形そのままにプログラミングすることができるようになってます. 支配方式の形をそのままプログラムソースに反映させられるのでプログラムの可読性を向上させることができます. また, spml の配列関数は入出力配列の性質が名前からわかるようにするべく統一的に命名法にしたがっているので, 関数の使い方が機械的になり, プログラムを修正することも容易に行えるようになっています.

SPMODEL のライブラリとサンプルプログラム自体が Fortran90 のプログラミング書法の一つの実験でもあります. 地球流体力学の問題に限らずさまざまな物理現象のスペクトル法による数値計算の一つのスタイルとして参考にして頂ければと思います.

=end JA
=begin EN
== The goal of SPMODEL

SPMODEL project is developing a series of numerical models based on various systems of approximate equations, that appear in the geophysical fluid dynamics.
The model series is arranged hierarchically from simple spartial one-dimensinal models to complex two or three dimensional models.

The aims of the series of numerical models are as follows:
* Easy reproduction of illustrations of standard or important GFD problems
  in order to make use of understandings and teaching
  of geophysical fluid dynamics.
* Sharing knowledge obtained by numerical experiments with ease:
  from the era of unserstanding with conventional mathematical equations
  to the era of understanding through the results of numerical calculations.
* Smooth comparison bwtween the numerical results of the series of the models:
  it is helpful in understanding of the results of more complex models.

For such purposes, readability and understandability of the source codes to permit easy re-building and/or modification are given priority in designing the program source codes. This policy might sacrifice calculation speed of the programs, however, we anticipate that on the basis of this model series,
advanced models, which can perform leading-edge massive calculations for exapmle, could be developed easily.

The programs of the series of the models use "SPMODEL library (spml)", which provides basic functions for numerical fluid dynamics calculations with the spectral methods, such as conversion between grid and spectral data and spartial derivatives. By using the array-valued functions of SPMODEL library under favor of
the array operation features of Fortran90, the main part of each program source code is written in a similar form to the original mathematical expressions,
which contributes to readability and understandability of the program source codes. Moreover, with the help of the introduction of systematic function naming rules of the array-valued functions of SPMODEL library, we can use these functions routinely, and the program source codes can be modified easyly and safely.

The SPMODEL library and the sample programs serve as one of the experiments for Fortran90 programing style. We are pleased if we colud offer them as references for programing style of several numerical calculations not only in geophysical fluid dynamics but also in other several fields of phisics.

=end EN
