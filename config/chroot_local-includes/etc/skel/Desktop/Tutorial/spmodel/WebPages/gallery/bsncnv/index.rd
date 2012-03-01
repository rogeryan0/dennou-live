=begin JA
= 2 次元熱対流(ベナール・レイリー対流など
=end JA
=begin EN
= 2D thermal convection (Benard-Rayleigh problem)
=end EN

=begin
#  * 2009/01/15 (竹広真一): spmodel 内相対パスに変更
#  * 2007/12/05 (佐々木洋平) Makefile.rd2html の更新に伴う修正
#  * 2005/07/22 (小高正嗣): 更新
#  * 2005/06/28 (小高正嗣): 最終更新
#  * 2002/10/03 (竹広真一): 新規作成
=end


=begin HTML
<br>
<a href="./bsncnv-tt-1-anim.gif"><IMG src="./bsncnv-tt-1.gif"></a>
=end HTML

=begin JA

* ブシネスク流体の熱対流の温度分布. 
* ベナール・レイリー対流の設定. 
  上下境界面で温度固定, 内部熱源なし, 応力なし条件(free-slip).
* 横方向は周期的境界条件. 
* 下面で温められた流体は軽くなり上昇しようとする. 
  上面で冷やされた流体は重くなり下降しようとする. 
* 上昇流・下降流のところにプルームができる. 
* ソースプログラムは((<こちら|URL:../../2d-channel-esc/boussinesq/sample-tt/f90/bsncnv-tt-1.f90>))

=end JA
=begin EN

* The left figure is temperature distribution 
  of boussinesq fluid heated from lower boundary
  (Benard-Rayleigh problem).

* On the lower and upper boundary, 
  temperature is fixed and tangential stress is equal to zero
  (free-slip boundary condition). 

* The horizontal boundary condition is cyclic.

* Click figure to start movie.

* ((<Source code|URL:../../2d-channel-esc/boussinesq/sample-tt/f90/bsncnv-tt-1.f90>))

=end EN
=begin HTML
<hr>
<a href="./bsncnv-tt-2-anim.gif"><IMG src="./bsncnv-tt-2.gif"></a>
=end HTML

=begin JA

* より横長な領域での計算.

* プルームの間隔(対流セルの水平スケール)は流体層の厚さと同程度.

* ソースプログラムは ((<<こちら|URL:../../2d-channel-esc/boussinesq/sample-tt/f90/bsncnv-tt-2.f90>))

=end JA
=begin EN

* The same as upper figure but with large horizontal domain.

* Click figure to start movie.

* ((<Source code|URL:../../2d-channel-esc/boussinesq/sample-tt/f90/bsncnv-tt-2.f90>))

=end EN

=begin HTML
<hr>
<a href="./bsncnv-ff-1-anim.gif"><IMG src="./bsncnv-ff-1.gif"></a>
=end HTML

=begin JA

* 上のベナール・レイリー対流の設定に対して, 
  温度ではなく一様一定な熱フラックスを上下境界で与えた場合.

* プルームの横方向の間隔が大きくなる. 非常に横長の対流セルが生じる.

* ソースプログラムは ((<こちら|URL:../../2d-channel-esc/boussinesq/sample-ff/f90/bsncnv-ff-1.f90>))

=end JA
=begin EN

* The left figure is temperature distribution of 
  boussinesq fluid heated from lower boundary
  with constant heat flux. 

* Click figure to start movie.

* The aspect ratio of convection cell becomes large as time goes on.

* ((<Source code|URL:../../2d-channel-esc/boussinesq/sample-ff/f90/bsncnv-ff-1.f90>))

=end EN

=begin JA
=== 参考文献

ベナール・レイリー対流については多くの標準的な(地球)流体力学の教科書に
解説がある. 例えば, 

* 木村竜治, 1983 : 地球流体力学入門, 東京堂出版, pp.247.

* Chandrasekhar, S., 1961 : Hydrodynamic and Hdromagnetic stability.
  Oxford University Press.

熱境界条件の影響については, 例えば, 

* Ishiwatari, M., Takehiro, S., Hayashi, Y.-Y., 1994 : 
  The effects of thermal conditions on the cell sizes of two-dimensional convection. 
  J. Fluid Mech., 281, 33--50. 

を参考にされたい. 

=end JA
=begin EN

=== References

* Chandrasekhar, S., 1961 : Hydrodynamic and Hdromagnetic stability.
  Oxford University Press.

* Ishiwatari, M., Takehiro, S., Hayashi, Y.-Y., 1994 : 
  The effects of thermal conditions on the cell sizes of two-dimensional convection. 
  J. Fluid Mech., 281, 33--50. 

=end EN

