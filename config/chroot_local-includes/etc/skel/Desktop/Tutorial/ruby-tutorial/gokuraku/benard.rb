#! /usr/bin/env ruby

require 'narray'
require "numru/dcl"

include NumRu
include NMath

n = 100
x = NArray.float(n).random!(0.1) + 0.02
y = NArray.float(n).random!(0.02) + 0.9
               # self.random!(max)  0��x��max �ΰ��ͤʥ������ͤ�����

def uv(x,y,t)  # (x,y,t)�ˤ�����®�٤����
  a = 1.0      # ����Ȥο���
  k = 2.0      # ������ȿ�
  e = 0.2      # ����ο���
  omega = PI*2
  u =  a*cos(PI*y)*sin(k*x) - e*cos(PI*y)*cos(k*x)*cos(omega*t)
  v = -a*sin(PI*y)*cos(k*x) + e*sin(PI*y)*sin(k*x)*cos(omega*t)
  return u,v
end

def advect(x,y,t,dt) # Runge-Kutta
  u1,v1 = uv(x, y, t)
  u2,v2 = uv(x+u1*dt/2.0, y+v1*dt/2.0, t+dt/2.0)
  u3,v3 = uv(x+u2*dt/2.0, y+v2*dt/2.0, t+dt/2.0)
  u4,v4 = uv(x+u3*dt, y+v3*dt, t+dt)

  x = x + (u1+u2*2+u3*2+u4)/6 * dt
  y = y + (v1+v2*2+v3*2+v4)/6 * dt
  t = t + dt

  return x,y,t
end

DCL::swiset('iwidth',512)      # ������ɥ��Ȥ��礭������
DCL::swiset('iheight',512)
if ARGV.index("-anim") then    # ���˥᡼����󤹤��������
  DCL::swlset('lwait',false)
  DCL::swlset('lalt',true)
end
DCL::gropn(1)

t = 0.0
dt = 0.05
for i in 1..1000
  x,y,t = advect(x,y,t,dt)

  x[ x < 0.0 ] = x[ x < 0.0 ] + PI       # Cyclic�ν���
  x[ x > PI ]  = x[ x > PI ]  - PI

  if ( ARGV.index("-anim") || i%5 == 0 )
    DCL::grfrm
    DCL::grswnd( 0.0, PI, 0.0, 1.0 )
    DCL::grsvpt( 0.1, 0.9, 0.2, 0.7 )
    DCL::grstrf
    DCL::usdaxs

    DCL::sgspmi(22)                      # ���ο�������������
    DCL::sgpmu( x, y )                   # ��ɸ(x,y)�������Ǥ�
  end
end

DCL::grcls

