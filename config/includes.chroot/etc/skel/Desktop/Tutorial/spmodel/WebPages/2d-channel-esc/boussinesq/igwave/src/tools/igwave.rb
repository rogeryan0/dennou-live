=begin
  ɽ��  igwave.rb

  �Ȥ���: ruby igwave.rb 

  ����  2004/04/29  ��������: gave ������
=end 
require "numru/gphys"
require "numru/dcl"
require "colorbar" 

include NumRu


filename = "igwave1-omega0.3.nc"
varname = "rho"
gphys = GPhys::NetCDF_IO.open(filename,varname)
gphys = gphys[0..-1,0..-1,-1]

##### ���顼�С�������

levels = NArray[-1000,-200,-100, 0, 100, 200, 300,1000]
patterns = NArray[30999, 35999, 40999, 55999, 70999, 75999, 85999]

# ���顼�С�������
$cbar_conf = {
  "vx0"=>0.1,            # ���顼�С��κ����Ѥ� x ��ɸ
  "vy0"=>0.25,            # ���顼�С��κ����Ѥ� y ��ɸ
  "vxlength"=>0.3,      # ���顼�С��� ����Ĺ��
  "vylength"=>0.02,      # ���顼�С��� �Ĥ�Ĺ��
  "tick1"  => 5,         # ����(��)���դ���ֳ�
  "tick2"  => 20,        # ����(��)���դ���ֳ�
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }

##############


iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(iws)

xmin = 0.0
xmax = 1.984375
ymin = 0.0
ymax = 1.0

vxmin = 0.2
vxmax = 0.8
vymin = 0.35
vymax = 0.65

x_title = "x"
x_unit = "m"
y_title = "y"
y_unit = "m"

DCL::sglset("LCNTL", false )
DCL::sglset("LCORNER", false )
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
## case1 �ξ��
DCL::uegtla(-4.0e-05, 4.0e-05, -80 )
## case2 �ξ��
#DCL::uegtla(-8.0e-05, 8.0e-05, -80 )
## case3 �ξ��
#DCL::uegtla(-2.0e-04, 2.0e-04, -80 )
## case4 �ξ��
#DCL::uegtla(-1.0e-04, 1.0e-04, -80 )
DCL::uetonf(gphys.val)
DCL::ussttl(x_title, x_unit, y_title, y_unit)
DCL::usdaxs
DCL::sglset("LCLIP", false )
i=0
DCL::sgtxzv(vxmax+0.01,vymax-0.03*i,"t=400 s",0.02,0,-1,1)
DCL::uzrset("ROFFXT", 0.06)
title = "density (kg/m3)"
DCL::uxsttl("t", title, 0 )

DCL::Util::color_bar($cbar_conf)

DCL::grcls

