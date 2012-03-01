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
varname_exact = "zeta_exact"
varname = "zeta"

exact  = GPhys::NetCDF_IO.open(filename,varname_exact)
result = GPhys::NetCDF_IO.open(filename,varname)


DCL::gropn(2)
DCL.sglset( 'lcorner', false )


xmin = 0.0
xmax = 4.921875
ymin = 0.0
ymax = 3.0

vxmin = 0.2
vxmax = 0.8
vymin = 0.2
vymax = 0.8

x_title = "x"
x_unit = "1"
y_title = ""
y_unit = ""

DCL::sglset("LCNTL", false )
DCL::udlset("LMSG", false )
DCL::gllset("LMISS", true )

DCL::grfrm
DCL::grswnd(xmin,xmax,ymin,ymax)
DCL::grsvpt(vxmin,vxmax,vymin,vymax)
DCL::grstrn(1)
DCL::grstrf

DCL::sglset("LCLIP", true )

##################################
exact1  = exact[0..-1,0]
result1 = result[0..-1,0]

DCL::sgplzu(exact1.coord(0).val, exact1.val, 3, 3 )
DCL::sgplzu(result1.coord(0).val, result1.val, 1, 3 )

exact1  = exact[0..-1,10]
result1 = result[0..-1,10]

DCL::sgplzu(exact1.coord(0).val, exact1.val, 3, 3 )
DCL::sgplzu(result1.coord(0).val, result1.val, 1, 3 )

exact1  = exact[0..-1,-1]
result1 = result[0..-1,-1]

DCL::sgplzu(exact1.coord(0).val, exact1.val, 3, 3 )
DCL::sgplzu(result1.coord(0).val, result1.val, 1, 3 )


DCL::ussttl(x_title, x_unit, y_title, y_unit)
DCL::usdaxs
DCL::sglset("LCLIP", false )
i=0
DCL::sgtxzv(vxmax+0.01,vymax-0.03*i,"",0.02,0,-1,1)
title = "C"
DCL::uysttl("l", title, 0 )

DCL::grcls

