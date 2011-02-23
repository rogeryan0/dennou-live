=begin JA
= 球面上の流れ
=end JA

=begin EN
= Shallow water flow on sphere
=end EN
=begin
#  * 2009/01/19 (竹広真一): サンプルプログラム場所変更
#  * 2009/01/15 (竹広真一): spmodel 内相対パスに変更
#  * 2007/12/05 (佐々木洋平) Makefile.rd2html の更新に伴う修正
#  * 2006/05/02 (佐々木洋平): 更新
#  * 2005/10/07 (竹広真一): 更新
#  * 2005/07/22 (小高正嗣): 更新
#  * 2005/06/28 (小高正嗣): 最終更新
#  * 2002/10/03 (竹広真一): 新規作成
=end
=begin HTML
<br>
<a href="./shallow_topo_linear-anim.gif">
<IMG src="./shallow_topo_linear.gif"></a>
=end HTML

=begin JA

* 回転球面上の浅水系を用いて山岳に対する流れの応答を研究した
  Gross and Hoskins (1979) の計算を追試したもの. 

* 緯度 30 度, 経度 180 度を中心に半径 45 度の局所的な山をおいている.

* 剛体回転分布の西風(東向きの流れ)を与えたときの渦度擾乱の分布.

* ソースプログラムは ((<こちら|URL:../../2d-sphere-w/shallow/f90/spshallow_topo_linear.f90>))


=end JA
=begin EN

* Recalculration of Gross and Hoskins (1979) 

* ((<Source code|URL:../../2d-sphere-w/shallow/f90/spshallow_topo_linear.f90>))

=end EN

=begin HTML
<hr>
<a href="./shallow_topo_linear-sphere-anim.gif">
<IMG src="./shallow_topo_linear-sphere.gif"></a>
=end HTML

=begn JA
* 投影法を変えて表示したもの(正射図法).
=end JA

=begin EN

=end EN


=begn JA
=== 参考文献

* Grose, W. L., Hoskins, B. J., 1979 : 
  On the influence of orography on large-scale atmospheric flow. 
  J. Atmos. Sci., 36, 223--234. 
	 
=end JA
=begin EN

=== References

* Grose, W. L., Hoskins, B. J., 1979 : 
  On the influence of orography on large-scale atmospheric flow. 
  J. Atmos. Sci., 36, 223--234. 


=end EN

