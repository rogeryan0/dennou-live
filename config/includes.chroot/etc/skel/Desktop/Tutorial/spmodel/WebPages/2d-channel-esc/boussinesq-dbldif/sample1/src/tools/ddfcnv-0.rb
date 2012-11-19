=begin
ɽ��   ddfcnv-0.rb

�Ȥ��� ruby ddfcnv-0.rb

����   2004/06/03  ��������: gave ������
       2004/06/17  ��������
=end 
require "numru/gphys"
require "numru/dcl"
require "colorbar" 

include NumRu

filename = "ddfcnv_0.nc"

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(-iws)
DCL::sldiv('T', 1, 4)

xmin = 0.0
xmax = 1.96875
ymin = 0.0
ymax = 1.0

vymin = 0.1
vymax = 0.275

x_title = ""
x_unit = "x"
y_title = ""
y_unit = "y"

DCL::sglset("LCORNER", false )
DCL::sglset("LCNTL", false )
DCL::udlset("LMSG", false )
DCL::gllset("LMISS", true )
DCL::sglset('LFULL', true)
DCL::uzfact(0.6)

for j in [0, 10, 14, 18]

  varname = "comp"
  Comp = GPhys::NetCDF_IO.open(filename,varname)
  Comp = Comp[0..-1,0..-1,j]

  vxmin = 0.15
  vxmax = 0.5

# ���顼�С������� ###################################################
$cbar_conf = {
  "vx0"=>0.15,            # ���顼�С��κ����Ѥ� x ��ɸ
  "vy0"=>0.05,            # ���顼�С��κ����Ѥ� y ��ɸ
  "vxlength"=>0.2,      # ���顼�С��� ����Ĺ��
  "vylength"=>0.02,      # ���顼�С��� �Ĥ�Ĺ��
  "tick1"  => 1,         # ����(��)���դ���ֳ�
  "tick2"  => 2,        # ����(��)���դ���ֳ�
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#########################################################################

  DCL::grfrm
  DCL::grswnd(xmin,xmax,ymin,ymax)
  DCL::grsvpt(vxmin,vxmax,vymin,vymax)
  DCL::grstrn(1)
  DCL::grstrf

  DCL::sglset("LCLIP", true )
  DCL::uwsgxa(Comp.coord(0).val)
  DCL::uwsgya(Comp.coord(1).val)
  DCL::ueitlv
  DCL::uegtla(0.0, 1.0, 0 )
  DCL::uetonf(Comp.val)
  DCL::udgcla(0.0, 1.0, 0 )
  DCL::udcntz(Comp.val)
  DCL::ussttl(x_title, x_unit, y_title, y_unit)
  DCL::usdaxs
  DCL::sglset("LCLIP", false )
  DCL::uzrset("ROFFXT", 0.06)
  DCL::Util::color_bar($cbar_conf)

  varname = "psi"
  Psi = GPhys::NetCDF_IO.open(filename,varname)
  Psi = Psi[0..-1,0..-1,j]

  vxmin = 0.55
  vxmax = 0.9

# ���顼�С������� ###################################################
$cbar_conf = {
  "vx0"=>0.55,            # ���顼�С��κ����Ѥ� x ��ɸ
  "vy0"=>0.05,            # ���顼�С��κ����Ѥ� y ��ɸ
  "vxlength"=>0.2,      # ���顼�С��� ����Ĺ��
  "vylength"=>0.02,      # ���顼�С��� �Ĥ�Ĺ��
  "tick1"  => 1,         # ����(��)���դ���ֳ�
  "tick2"  => 2,        # ����(��)���դ���ֳ�
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#########################################################################

  DCL::grfig
  DCL::grswnd(xmin,xmax,ymin,ymax)
  DCL::grsvpt(vxmin,vxmax,vymin,vymax)
  DCL::grstrn(1)
  DCL::grstrf

  DCL::sglset("LCLIP", true )
  DCL::uwsgxa(Psi.coord(0).val)
  DCL::uwsgya(Psi.coord(1).val)
  DCL::ueitlv
  DCL::uegtla(-2.39315128, 2.39315128, 0 )
  DCL::uetonf(Psi.val)  
  DCL::udgcla(-2.39315128, 2.39315128, 0 )
  DCL::udcntz(Psi.val)
  DCL::ussttl(x_title, x_unit, y_title, y_unit)
  DCL::usdaxs
  DCL::sglset("LCLIP", false )
  DCL::uzrset("ROFFXT", 0.06)
  DCL::Util::color_bar($cbar_conf)

end

DCL::grcls

