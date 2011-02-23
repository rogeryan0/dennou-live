#! /usr/bin/env ruby
#
# KdV 方程式を解く＋解の図示(アニメ)
#
# ●configuration 
#    方程式 ; ∂u/∂t + u ∂u / ∂ x + ν ∂^3 u / ∂ x^3  = 0 
#    領域   ; x = [0, π]  t = [0, TN] 
#    b.c.   ; periodic i.e. u(xmin,t) = u(xmax,t) 
#    i.c.   ; u(x,0) = cos x etc. 
# 
# ●差分
#    時間は RKG (4次) 
#    空間は中央 => 三階微分のところは一意
# 
# ●実験
#   ν, TN を固定し, 初期値をかえたとき, 解の時間発展を見る.
#   sin 型, ソリトン など. 
#
# ●推奨パラメータ
#   e.g. JMAX=100, nu=0.01, DT=1e-4  => 適宜調整する必要がある
# 
# 00/01/17 Taguchi 
#          赤堀さんソースをもとに改変
# 00/11/14 f90化
# 03/02/23 ruby化
#----------------------------------------------------------------------#
require 'narray'
require "numru/dcl"

include NumRu
include NMath

class NArray # NArrayクラスにメソッドを追加
  def cshift(n) # Fortran90のcshiftの代用品
    #if(n > x.size) ...
    y = self.dup
    y[n..-1] = self[0..-1-n]
    y[0..n-1] = self[-n..-1]
    return y
  end
end

class Field
  def initialize(x,u,nu)
    @x = x
    @u = u
    @nu = nu
    @dx = @x[1]-@x[0]
  end

  def d_dx(u) # 微分(中央差分)
    (u.cshift(1)-u.cshift(-1))/(2*@dx)
  end

  def d3_dx3(u) # 3階微分
    (u.cshift(2)-2*u.cshift(1)+2*u.cshift(-1)-u.cshift(-2))/(2*@dx**3)
  end

  def kdv(u) # KdV方程式
    -u * d_dx(u) - @nu * d3_dx3(u)
  end

  def integ(dt) # dtだけ時間積分(4次Runge-Kutta)
    du1 = kdv(@u)
    du2 = kdv(@u+du1*dt/2)
    du3 = kdv(@u+du2*dt/2)
    du4 = kdv(@u+du3*dt)
    @u = @u + (du1+du2*2+du3*2+du4)/6 * dt
  end

  def x
    return @x
  end

  def u
    return @u
  end
end


if ARGV.index("-anim") then
  DCL::swlset('lwait',false)
  DCL::swlset('lalt',true)
end

# 初期値の設定

jmax = 100
xmin = 0.0
xmax = PI
dt = 0.0002

dx = (xmax-xmin)/jmax
x = NArray.sfloat(jmax).indgen!*dx + xmin

nu = 0.005
u1 = 4.0
d1 = sqrt(12.0*nu/u1)
u2 = 1.0
d2 = sqrt(12.0*nu/u2)
u = u1/( cosh( (x-PI/2)/d1 ) )**2 + u2/( cosh( (x-PI/4)/d2 ) )**2

a = Field.new(x,u,nu)

DCL::gropn(1)
loop do
  DCL::grfrm
  DCL::grswnd(xmin,xmax,0.0,5.0)
  DCL::usgrph(a.x,a.u)
  50.times do a.integ(dt) end
end
DCL::grcls

