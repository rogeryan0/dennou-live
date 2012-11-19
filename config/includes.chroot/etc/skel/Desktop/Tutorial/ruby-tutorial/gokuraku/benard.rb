#! /usr/bin/env ruby

require 'narray'
require "numru/dcl"

include NumRu
include NMath

n = 100
x = NArray.float(n).random!(0.1) + 0.02
y = NArray.float(n).random!(0.02) + 0.9
               # self.random!(max)  0≦x＜max の一様なランダム値を生成

def uv(x,y,t)  # (x,y,t)における速度を求める
  a = 1.0      # 定常波の振幅
  k = 2.0      # 擾乱の波数
  e = 0.2      # 擾乱の振幅
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

DCL::swiset('iwidth',512)      # ウィンドウ枠の大きさ設定
DCL::swiset('iheight',512)
if ARGV.index("-anim") then    # アニメーションする場合の設定
  DCL::swlset('lwait',false)
  DCL::swlset('lalt',true)
end
DCL::gropn(1)

t = 0.0
dt = 0.05
for i in 1..1000
  x,y,t = advect(x,y,t,dt)

  x[ x < 0.0 ] = x[ x < 0.0 ] + PI       # Cyclicの処理
  x[ x > PI ]  = x[ x > PI ]  - PI

  if ( ARGV.index("-anim") || i%5 == 0 )
    DCL::grfrm
    DCL::grswnd( 0.0, PI, 0.0, 1.0 )
    DCL::grsvpt( 0.1, 0.9, 0.2, 0.7 )
    DCL::grstrf
    DCL::usdaxs

    DCL::sgspmi(22)                      # 点の色・太さの設定
    DCL::sgpmu( x, y )                   # 座標(x,y)に点を打つ
  end
end

DCL::grcls

