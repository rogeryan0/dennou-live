=begin
ɽ��   bsncnv-tt-1.rb

�Ȥ��� ruby bsncnv-tt-1.rb

����   2004/05/28  ��������: gave ������
=end 
require "numru/gphys"
require "numru/dcl"
require "colorbar" 

include NumRu


filename = "bsncnv-tt-1.nc"
varname = "temp"
Temp = GPhys::NetCDF_IO.open(filename,varname)
Temp = Temp[0..-1,0..-1,20]

varname = "psi"
Psi = GPhys::NetCDF_IO.open(filename,varname)
Psi = Psi[0..-1,0..-1,20]

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(iws)

xmin = 0.0
xmax = 1.96875
ymin = 0.0
ymax = 1.0

vxmin = 0.2
vxmax = 0.8
vymin = 0.6
vymax = 0.9

# ���顼�С������� ###################################################
$cbar_conf = {
  "vx0"=>0.1,            # ���顼�С��κ����Ѥ� x ��ɸ
  "vy0"=>0.52,            # ���顼�С��κ����Ѥ� y ��ɸ
  "vxlength"=>0.3,      # ���顼�С��� ����Ĺ��
  "vylength"=>0.02,      # ���顼�С��� �Ĥ�Ĺ��
  "tick1"  => 1,         # ����(��)���դ���ֳ�
  "tick2"  => 2,        # ����(��)���դ���ֳ�
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#########################################################################

x_title = ""
x_unit = "x"
y_title = ""
y_unit = "y"

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
DCL::uwsgxa(Temp.coord(0).val)
DCL::uwsgya(Temp.coord(1).val)
DCL::ueitlv
DCL::uegtla(0.0, 1.0, 0 )
DCL::uetonf(Temp.val)
DCL::udgcla(0.0, 1.0, 0 )
DCL::udcntz(Temp.val)
DCL::ussttl(x_title, x_unit, y_title, y_unit)
DCL::usdaxs
DCL::sglset("LCLIP", false )
i=0
DCL::sgtxzv(vxmax+0.01,vymax-0.03*i,"t=0.5",0.02,0,-1,1)
DCL::uzrset("ROFFXT", 0.06)
title = "T"
#DCL::uxsttl("t", title, 0 )

DCL::Util::color_bar($cbar_conf)

DCL::grfig

vxmin = 0.2
vxmax = 0.8
vymin = 0.15
vymax = 0.45

# ���顼�С������� ###################################################
$cbar_conf = {
  "vx0"=>0.1,            # ���顼�С��κ����Ѥ� x ��ɸ
  "vy0"=>0.08,            # ���顼�С��κ����Ѥ� y ��ɸ
  "vxlength"=>0.3,      # ���顼�С��� ����Ĺ��
  "vylength"=>0.02,      # ���顼�С��� �Ĥ�Ĺ��
  "tick1"  => 1,         # ����(��)���դ���ֳ�
  "tick2"  => 2,        # ����(��)���դ���ֳ�
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#########################################################################


DCL::grswnd(xmin,xmax,ymin,ymax)
DCL::grsvpt(vxmin,vxmax,vymin,vymax)
DCL::grstrn(1)
DCL::grstrf

DCL::sglset("LCLIP", true )
DCL::uwsgxa(Psi.coord(0).val)
DCL::uwsgya(Psi.coord(1).val)
DCL::ueitlv
DCL::uegtla(-18.3568261, 18.3568261, 0 )
DCL::uetonf(Psi.val)
DCL::udgcla(-18.3568261, 18.3568261, 0 )
DCL::udcntz(Psi.val)
DCL::ussttl(x_title, x_unit, y_title, y_unit)
DCL::usdaxs
DCL::sglset("LCLIP", false )
i=0
DCL::sgtxzv(vxmax+0.01,vymax-0.03*i,"t=0.5",0.02,0,-1,1)
DCL::uzrset("ROFFXT", 0.06)
title = "Stream function"
#DCL::uxsttl("t", title, 0 )

DCL::Util::color_bar($cbar_conf)



DCL::grcls

