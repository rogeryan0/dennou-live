require "narray"
require "numru/ssl2"
require "eof_ssl2.rb"
require "numru/dcl"

include NumRu

n = 1000
x = NArray.float(2,n)

a,ix = SSL2.rann1(0,0.4,0,n)
b, = SSL2.rann1(0,0.2,ix,n)

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
DCL::sgpmzu(x[0,true],x[1,true],1,1,0.01)
for i in 0..1
  fact = val[i]
  DCL::sgplzu([-vec[0,i]*fact,vec[0,i]*fact],[-vec[1,i]*fact,vec[1,i]*fact],1,3)
end
DCL::grcls
