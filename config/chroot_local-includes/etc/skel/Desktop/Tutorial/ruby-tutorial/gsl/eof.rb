require "narray"
require "gsl"
require "eof_gsl.rb"
require "numru/dcl"

include NumRu

def rand_nmal(m, s, n)

  # Àµµ¬Íð¿ôºîÀ®
  rnd_std_nmal = NArray.sfloat(n).randomn 
  rnd_nmal     = rnd_std_nmal * s + m 

  return rnd_nmal
end

n = 1000
x = NArray.float(2,n)

a = rand_nmal(0,0.4,n)  # Ê¿¶Ñ 0, Ê¬»¶ 0.4 ¤ÎÀµµ¬Íð¿ô
b = rand_nmal(0,0.2,n)  # Ê¿¶Ñ 0, Ê¬»¶ 0.2 ¤ÎÀµµ¬Íð¿ô

theta = Math::PI/6
x[0,true] = a*Math::cos(theta) - b*Math::sin(theta)
x[1,true] = a*Math::sin(theta) + b*Math::cos(theta)

val,vec = Analysis.eof(x)

DCL::gropn(1)
DCL::grfrm
DCL::grswnd(-1.5,1.5,-1.5,1.5)
DCL::uspfit
DCL::grstrf
DCL::usdaxs
DCL::sgpmzu(x[0, true],x[1,true],1,1,0.01)
for i in 0..1
  fact = val[i]
  DCL::sgplzu([-vec[0,i]*fact,vec[0,i]*fact],[-vec[1,i]*fact,vec[1,i]*fact],1,3)
end
DCL::grcls
