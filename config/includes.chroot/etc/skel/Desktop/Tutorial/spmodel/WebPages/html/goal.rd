=begin JA
= SPMODEL の目指すもの
=end JA
=begin EN
= The goal of SPMODEL
=end EN
=begin

# * 2012/02/27 (佐々木洋平) 英語修正追記
# * 2007/12/05 (佐々木洋平) Makefile.rd2html の更新に合わせた修正
# * 2005/07/21 (小高正嗣) 最終更新
# * 2005/06/30 (小高正嗣) 最終更新
# * 2005/04/01 (小高正嗣) 最終更新
# * 2005/04/01 (小高正嗣)
# * 2005/03/30 (小高正嗣)
# * 2004/03/18 (小高正嗣)
# * 2004/01/26 (小高正嗣)
# * 2002/09/12 (竹広真一, 林祥介)
# * 2002/02/15 (竹広真一) 新規作成

=end

=begin JA

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

Fortran90 の配列機能を生かしたこのライブラリの配列関数を用いることで, 時間発展方程式の時間変化項以外の部分を数式の形そのままにプログラミングすることができるようになってます. 支配方程式の形をそのままプログラムソースに反映させられるのでプログラムの可読性を向上させることができます. また, spml の配列関数は入出力配列の性質が名前からわかるようにするべく統一的に命名法にしたがっているので, 関数の使い方が機械的になり, プログラムを修正することも容易に行えるようになっています.

SPMODEL のライブラリとサンプルプログラム自体が Fortran90 のプログラミング書法の一つの実験でもあります. 地球流体力学の問題に限らずさまざまな物理現象のスペクトル法による数値計算の一つのスタイルとして参考にして頂ければと思います.


=end JA
=begin EN

SPMODEL project is developing a series of numerical models
based on various systems of approximate equations,
that appear in the geophysical fluid dynamics.
The model series is arranged hierarchically from simple
spartial one-dimensinal models to complex
two or three dimensional models.

The aims of the series of numerical models are as follows:
* Easy reproduction of illustrations of standard or important GFD problems
  in order to make use of understandings and teaching
  of geophysical fluid dynamics.
* Sharing knowledge obtained by numerical experiments with ease:
  from the era of unserstanding with conventional mathematical equations
  to the era of understanding through the results of numerical calculations.
* Smooth comparison bwtween the numerical results of the series of the models:
  it is helpful in understanding of the results of more complex models.

For such purposes, readability and understandability of the source codes
to permit easy re-building and/or modification
are given priority in designing the program source codes.
This policy might sacrifice calculation speed of the programs, however,
we anticipate that on the basis of this model series,
advanced models,
which can perform leading-edge massive calculations for exapmle,
could be developed easily.

The programs of the series of the models use "SPMODEL library (spml)",
which provides basic functions for numerical fluid dynamics calculations
with the spectral methods, such as conversion between grid and
spectral data and spartial derivatives.
By using the array-valued functions of SPMODEL library under favor of
the array operation features of Fortran90,
the main part of each program source code is written
in a similar form to the original mathematical expressions,
which contributes to readability and understandability
of the program source codes.
Moreover, with the help of the introduction
of systematic function naming rules of the array-valued functions
of SPMODEL library, we can use these functions routinely,
and the program source codes can be modified easyly and safely.

The SPMODEL library and the sample programs serve as one of
the experiments for Fortran90 programing style.
We are pleased if we colud offer them as references for programing style of
several numerical calculations not only in geophysical fluid dynamics
but also in other several fields of phisics.

=end EN

