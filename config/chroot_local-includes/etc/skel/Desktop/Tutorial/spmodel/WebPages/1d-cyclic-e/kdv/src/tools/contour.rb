#  表題  contour.rb 
#
#  使い方: ruby contour.rb [filename]
#
#  履歴  2004/03/19  小高正嗣: gave より作成
#

require "numru/gphys"
require "numru/dcl"

include NumRu


filename = ARGV[0]
varname = "zeta"
gphys = GPhys::NetCDF_IO.open(filename,varname)
gphys = gphys[0..-1,0..-1]

DCL::gropn(2)

xmin = 0.0
xmax = 2.9765625
ymin = 0.0
ymax = 0.004999999888

vxmin = 0.2
vxmax = 0.8
vymin = 0.2
vymax = 0.8

x_title = "x"
x_unit = ""
y_title = "t"
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
DCL::uwsgxa(gphys.coord(0).val)
DCL::uwsgya(gphys.coord(1).val)
DCL::ueitlv
#DCL::uegtla(-0.03416405276, 1440.025902, 0 )
#DCL::uetonf(gphys.val)
DCL::udgcla(-0.03416405276, 1440.025902, 0 )
DCL::udcntz(gphys.val)
DCL::ussttl(x_title, x_unit, y_title, y_unit)
DCL::usdaxs
DCL::sglset("LCLIP", false )
DCL::uzrset("ROFFXT", 0.06)
title = DCL::csgi(157)
DCL::uxsttl("t", title, 0 )

DCL::grcls

