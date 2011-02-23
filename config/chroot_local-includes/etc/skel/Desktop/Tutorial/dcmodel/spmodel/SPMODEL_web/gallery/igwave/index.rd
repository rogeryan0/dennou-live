=begin JA
= 内部重力波
=end JA
=begin EN
= Internal gravity wave
=end EN
=begin
#  * 2009/01/15 (竹広真一): spmodel 内相対パスに変更
#  * 2007/12/05 (佐々木洋平) Makefile.rd2html の更新に伴う修正
#  * 2006/05/02 (佐々木洋平): 更新
#  * 2005/07/22 (小高正嗣): 更新
#  * 2005/06/28 (小高正嗣): 最終更新
#  * 2002/10/03 (竹広真一): 新規作成
=end
=begin HTML
<br>
<a href="./igwave1-omega0.3-anim.gif"><IMG src="./igwave1-omega0.3.gif"></a>
=end HTML

=begin JA

* 連続成層流体中で円柱を上下に振動させて内部重力波を作る室内実験を模したもの.

* 円柱の振動の代わりに, 
  水平方向に正と負が並んだ双極子な分布を持つ渦度の強制を
  成層している流体層の中心に与えている. 

* 右図は強制の振幅の振動数がブラントバイサラ振動数の 0.3 倍の場合の密度擾乱の分布. 

* ×の字パターンが開いている. 

* ((<「地球流体力学実験集」|URL:/library/gfd_exp/exp_j/doc/iw/guide01.htm>))
  の室内実験もあわせて御覧あれ.

* ソースプログラムは((<こちら|URL:../../2d-channel-esc/boussinesq/igwave/f90/igwave1.f90>))

=end JA
=begin EN

* ((<Source code|URL:../../2d-channel-esc/boussinesq/igwave/f90/igwave1.f90>))

=end EN
=begin HTML
<hr>
<a href="./igwave1-omega0.6-anim.gif"><IMG src="./igwave1-omega0.6.gif"></a>
=end HTML

=begin JA

* 強制の振幅の振動数がブラントバイサラ振動数の 0.6 倍の場合の密度擾乱の分布. 

* 振動数が大きくなると, ×の字パターンが閉じていく. 

=end JA
=begin EN


=end EN

=begin HTML
<hr>
<a href="./igwave1-omega0.9-anim.gif"><IMG src="./igwave1-omega0.9.gif"></a>
=end HTML

=begin JA

* 強制の振幅の振動数がブラントバイサラ振動数の 0.9 倍の場合の密度擾乱の分布.

* さらに×の字パターンが閉じていく. 

=end JA
=begin EN

=end EN


=begin HTML
<hr>
<a href="./igwave1-omega1.2-anim.gif"><IMG src="./igwave1-omega1.2.gif"></a>
=end HTML

=begin JA

* 強制の振幅の振動数がブラントバイサラ振動数の 1.2 倍の場合の密度擾乱の分布.

* ブラントバイサラ振動数より, 強制の振動数が大きくなるともはや波が発生しなくなる. 
=end JA



=begin JA
=== 参考文献

多くの地球流体力学の教科書に内部重力波の解説があるだろう. 
特に室内実験の説明に詳しいものとしては, 


* 木村竜治, 1983 : 地球流体力学入門, 東京堂出版, pp.247.

* GFD-online (酒井敏，飯澤功，荒牧英治), 1997 : 
  実験室の中の空と海, 内部重力波, 
  ((<"http://www.gfd-dennou.org/library/gfd_exp/exp_j/doc/iw/guide01.htm"|URL:/library/gfd_exp/exp_j/doc/iw/guide01.htm>))
	 
* Mowbray, D. E., Rarity, B. S. H., 1967 : 
  A theoretical and experimental investigation of the phase configuration ofinternal waves of small amplitude in a density stratified liquid. 
  J. Fluid  Mech., 28, 1--16.

=end JA
=begin EN

=== References

* GFD-online (S. Sakai, I, Iizawa, E. Aramaki), 1997:
  Atmosphere and Ocean in a Laboratory: 
  The happy deep-sea fish
  ((<"http://www.gfd-dennou.org/library/gfd_exp/exp_e/doc/iw/guide01.htm"|URL:/library/gfd_exp/exp_e/doc/iw/guide01.htm>))


* Mowbray, D. E., Rarity, B. S. H., 1967 : 
  A theoretical and experimental investigation of the phase configuration ofinternal waves of small amplitude in a density stratified liquid. 
  J. Fluid  Mech., 28, 1--16.

=end EN
