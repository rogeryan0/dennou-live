=begin JA
= K-H 不安定
=end JA
=begin EN
= Kelvin-Helmholtz instability
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
<a href="./kh1-anim.gif"><IMG src="./kh1.gif"></a>
=end HTML

=begin JA

* 密度の小さい上層(青色)は右向きに, 
  密度の大きな下層(赤色)が左向きにほぼ一様に流れている. 

* 横方向は周期的境界条件. 

* 上下の速度差が大きいと平行に流れていられなくなり, 
  密度境界面が変形して行く.

* ((<「地球流体力学実験集」|URL:/library/gfd_exp/exp_j/doc/kh/guide01.htm>))
  の室内実験も御覧あれ.
	      
* ソースプログラムは((<こちら|URL:../../2d-channel-esc/boussinesq/kh-instability/f90/kh1.f90>))


=== 参考文献

* Chandrasekhar, S., 1961 : Hydrodynamic and Hdromagnetic stability.
  Oxford University Press.

* GFD-online (酒井敏，飯澤功，荒牧英治), 1997 : 
  実験室の中の空と海, K-H 不安定, 
  ((<"http://www.gfd-dennou.org//arch/gfd-exp/gfd_exp/exp_j/doc/kh/guide01.htm"|URL:/library/gfd_exp/exp_j/doc/kh/guide01.htm>))
	 
=end
=begin EN

* The upper low-density fluid (blue) flows from left to right
  and lower high-density fluid (red) flows right to left.
  Each flow is uniform vertically. 

* The horizontal boundary condition is cyclic. 

* Click figure to start movie.

* See also result of laboratory experiment presented by 
  ((<"Atmosphere and Ocean in a Laboratory"|URL:/library/gfd_exp/exp_e/doc/kh/guide01.htm>)).
	      
* ((<Source code|URL:../../2d-channel-esc/boussinesq/kh-instability/f90/kh1.f90>))


=== References

* Chandrasekhar, S., 1961 : Hydrodynamic and Hdromagnetic stability.
  Oxford University Press.

* GFD-online (S. Sakai, I, Iizawa, E. Aramaki), 1997:
  Atmosphere and Ocean in a Laboratory: 
  Hokusai
  ((<"http://www.gfd-dennou.org/arch/gfd_exp/exp_e/doc/kh/guide01.htm"|URL:/library/gfd_exp/exp_e/doc/kh/guide01.htm>))

=end EN
