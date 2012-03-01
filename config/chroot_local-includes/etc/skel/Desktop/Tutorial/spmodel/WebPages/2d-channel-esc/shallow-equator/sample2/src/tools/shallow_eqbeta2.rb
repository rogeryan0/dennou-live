=begin
ɽ��   shallow_eqbeta2.rb

�Ȥ��� ruby shallow_eqbeta2.rb

����   2004/06/10  ��������: gave ������
=end 
require "numru/gphys"
require "numru/dcl"
require "colorbar" 

include NumRu


filename = "shallow_eqbeta2.nc"

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(-iws)
DCL::sldiv('T', 2, 5)

xmin = -200.0
xmax = 193.75
ymin = -5.0
ymax = 5.0

vxmin = 0.1
vxmax = 0.9
vymin = 0.15
vymax = 0.45

# ���顼�С������� ###################################################
$cbar_conf = {
  "vx0"=>0.05,            # ���顼�С��κ����Ѥ� x ��ɸ
  "vy0"=>0.05,            # ���顼�С��κ����Ѥ� y ��ɸ
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
DCL::sglset('LFULL', true)

for j in 1..10

  varname = "height"
  Hight = GPhys::NetCDF_IO.open(filename,varname)
  Hight = Hight[0..-1,0..-1,2*j]

  DCL::grfrm
  DCL::grswnd(xmin,xmax,ymin,ymax)
  DCL::grsvpt(vxmin,vxmax,vymin,vymax)
  DCL::grstrn(1)
  DCL::grstrf

  DCL::sglset("LCLIP", true )
  DCL::uwsgxa(Hight.coord(0).val)
  DCL::uwsgya(Hight.coord(1).val)
  DCL::ueitlv
  DCL::uegtla(-5.0, 5.5, 0.5 )
  DCL::uetonf(Hight.val)
  DCL::udgcla(-5.0, 5.5, 1.0 )
  DCL::udcntz(Hight.val)
  DCL::ussttl(x_title, x_unit, y_title, y_unit)
  DCL::usdaxs
  DCL::sglset("LCLIP", false )
  i=0
#  DCL::sgtxzv(vxmax+0.01,vymax-0.03*i,"t=0.5",0.01,0,-1,1)
  DCL::uzrset("ROFFXT", 0.06)
  title = "Hight"
#  DCL::uxsttl("t", title, 0 )
  DCL::Util::color_bar($cbar_conf)
end


DCL::grcls

