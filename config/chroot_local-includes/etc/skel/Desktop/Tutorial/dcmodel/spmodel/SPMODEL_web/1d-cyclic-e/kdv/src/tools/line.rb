#  表題  line.rb 
#
#  使い方: ruby line.rb [filename]
#
#  履歴  2004/03/19  小高正嗣: gave より作成
#

require "numru/gphys"
require "numru/dcl"

include NumRu


filename = ARGV[0]
varname = "zeta"
gphys = GPhys::NetCDF_IO.open(filename,varname)
gphys = gphys[0..-1,0]

DCL::gropn(2)

xmin = 0.0
xmax = 2.9765625
ymin = 3.349574542e-07
ymax = 1440.025902

vxmin = 0.2
vxmax = 0.8
vymin = 0.2
vymax = 0.8

x_title = "x"
x_unit = ""
y_title = ""
y_unit = ""

DCL::sglset("LCORNER", false )
DCL::sglset("LCNTL", false )
DCL::udlset("LMSG", false )
DCL::gllset("LMISS", true )

DCL::grfrm
DCL::grswnd(xmin,xmax,ymin,ymax)
DCL::grsvpt(vxmin,vxmax,vymin,vymax)
DCL::grstrn(1)
DCL::grstrf

DCL::sglset("LCLIP", true )
DCL::sgplzu(gphys.coord(0).val, gphys.val, 1, 3 )
DCL::ussttl(x_title, x_unit, y_title, y_unit)
DCL::usdaxs
DCL::sglset("LCLIP", false )
i=0
DCL::sgtxzv(vxmax+0.01,vymax-0.03*i,"t=0.0",0.02,0,-1,1)
title = DCL::csgi(157)
DCL::uysttl("l", title, 0 )

DCL::grcls

