#! /usr/bin/env ruby
#
# KdV ��������򤯡ܲ�ο޼�(���˥�)
#
# ��configuration 
#    ������ ; ��u/��t + u ��u / �� x + �� ��^3 u / �� x^3  = 0 
#    �ΰ�   ; x = [0, ��]  t = [0, TN] 
#    b.c.   ; periodic i.e. u(xmin,t) = u(xmax,t) 
#    i.c.   ; u(x,0) = cos x etc. 
# 
# ����ʬ
#    ���֤� RKG (4��) 
#    ���֤���� => ������ʬ�ΤȤ���ϰ��
# 
# ���¸�
#   ��, TN ����ꤷ, ����ͤ򤫤����Ȥ�, ��λ���ȯŸ�򸫤�.
#   sin ��, ����ȥ� �ʤ�. 
#
# ���侩�ѥ�᡼��
#   e.g. JMAX=100, nu=0.01, DT=1e-4  => Ŭ��Ĵ������ɬ�פ�����
# 
# 00/01/17 Taguchi 
#          ���٤��󥽡������Ȥ˲���
# 00/11/14 f90��
# 03/02/23 ruby��
#----------------------------------------------------------------------#
require 'narray'
require "numru/dcl"

include NumRu
include NMath

class NArray # NArray���饹�˥᥽�åɤ��ɲ�
  def cshift(n) # Fortran90��cshift��������
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

  def d_dx(u) # ��ʬ(�����ʬ)
    (u.cshift(1)-u.cshift(-1))/(2*@dx)
  end

  def d3_dx3(u) # 3����ʬ
    (u.cshift(2)-2*u.cshift(1)+2*u.cshift(-1)-u.cshift(-2))/(2*@dx**3)
  end

  def kdv(u) # KdV������
    -u * d_dx(u) - @nu * d3_dx3(u)
  end

  def integ(dt) # dt����������ʬ(4��Runge-Kutta)
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

# ����ͤ�����

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

