# hop.rb

require "narray"
require "numru/dcl"

include NumRu
include NMath                        # NArray��Math�⥸�塼��

nmax = 400
dt = 2*PI/(nmax-1)
t = NArray.sfloat(nmax).indgen! * dt # t���޲��ѿ�(0��t��2��)
x = 1e2 *sin(4*t)
y = 1e-3*cos(5*t)+6

p x

DCL::gropn(1)
DCL::grfrm

DCL::ussttl('X-TITLE', 'x-unit', 'Y-TITLE', 'y-unit')
DCL::usgrph(x, y)

DCL::grcls
