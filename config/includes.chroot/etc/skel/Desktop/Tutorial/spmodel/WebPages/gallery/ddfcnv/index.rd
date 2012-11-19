=begin JA
= 2 重拡散対流
=end JA
=begin EN
= Double-diffusive convection
=end EN
=begin
#  * 2009/01/15 (竹広真一): spmodel 内相対パスに変更
#  * 2007/12/05 (佐々木洋平) Makefile.rd2html の更新に伴う修正
#  * 2005/07/22 (小高正嗣): 更新
#  * 2004/06/03 (小高正嗣): 新規作成
=end
=begin HTML
<br>
<a href="./ddfcnv_0-anim.gif"><IMG src="./ddfcnv_0.gif" width="450"></a>
=end HTML

=begin JA

* 2 成分拡散ブシネスク流体の 2 重拡散対流の塩分濃度分布. 

* 温度については安定, 塩分については不安定な成層を与える.
 
* 上下境界面で温度と塩分固定, 内部熱源なし, 応力なし条件(free-slip).

* 横方向は周期的境界条件. 

* エノキ茸のようなプルームができる. 

* ((<「地球流体力学実験集」|URL:/library/gfd_exp/exp_j/doc/sf/guide01.htm>)) の室内実験もあわせて御覧あれ.

* ソースプログラムは ((<こちら|URL:../../2d-channel-esc/boussinesq-dbldif/sample1/f90/ddfcnv_0.f90>))

=== 参考文献

* GFD-online (酒井 敏, 飯澤 功, 荒巻 英治), 1997:
  実験室の中の空と海, 海に生えるエノキ茸,
  ((<"http://www.gfd-dennou.org/library/gfd_exp/exp_j/doc/sf/guide01.htm"|URL:/library/gfd_exp/exp_j/doc/sf/guide01.htm>))

* Turner, I. S., 1973: 
  Buoyance effects in fluids, Cambridge University Press.

=end
=begin EN

* The left figure is salinity distribution 
  of boussinesq fluid 
  double-diffusive convection.

* The basic stratification is stable for temperature and 
  unstable for salinity.

* On the lower and upper boundary, 
  temperature and salinity is fixed and 
  tangential stress is equal to zero
  (free-slip boundary condition). 

* The horizontal boundary condition is cyclic. 

* Click figure to start movie.

* See also result of laboratory experiment presented by 
  ((<"Atmosphere and Ocean in a Laboratory"|URL:/library/gfd_exp/exp_e/doc/sf/guide01.htm>))
	      
* ((<Source code|URL:../../2d-channel-esc/boussinesq-dbldif/sample1/f90/ddfcnv_0.f90>))

=== References

* GFD-online (S. Sakai, I, Iizawa, E. Aramaki), 1997:
  Atmosphere and Ocean in a Laboratory: 
  Velvet Shanks the Grow in the Sea.
  ((<"http://www.gfd-dennou.org/library/gfd_exp/exp_e/doc/sf/guide01.htm"|URL:/library/gfd_exp/exp_e/doc/sf/guide01.htm>))
         
* Turner, I. S., 1973: 
  Buoyance effects in fluids, Cambridge University Press.

=end EN

