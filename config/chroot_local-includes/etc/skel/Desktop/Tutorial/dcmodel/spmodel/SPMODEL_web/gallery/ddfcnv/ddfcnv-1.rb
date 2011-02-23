=begin
ɽ��   ddfcnv-1.rb

�Ȥ��� ruby ddfcnv-1.rb

����   2004/06/03  ��������: gave ������
=end 
require "numru/gphys"
require "numru/dcl"
require "colorbar" 

include NumRu

filename = "ddfcnv_0.nc"

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i

xmin = 0.0
xmax = 1.96875
ymin = 0.0
ymax = 1.0

vxmin = 0.2
vxmax = 0.8
vymin = 0.25
vymax = 0.55

# ���顼�С������� ###################################################
$cbar_conf = {
  "vx0"=>0.05,            # ���顼�С��κ����Ѥ� x ��ɸ
  "vy0"=>0.10,            # ���顼�С��κ����Ѥ� y ��ɸ
  "vxlength"=>0.2,      # ���顼�С��� ����Ĺ��
  "vylength"=>0.02,      # ���顼�С��� �Ĥ�Ĺ��
  "tick1"  => 1,         # ����(��)���դ���ֳ�
  "tick2"  => 2,        # ����(��)���դ���ֳ�
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#########################################################################

x_title = "X-coordinate"
x_unit = "1"
y_title = "Y-coordinate"
y_unit = "1"

DCL::sglset("LCORNER", false )    # �����ʡ��οޤ�񤫤ʤ�
DCL::sglset("LCNTL", false )      # ����ʸ����ͭ���ˤ���
DCL::sglset('LFULL', true)        # ���̤��äѤ�������
DCL::gllset("LMISS", true )       # ��»����
DCL::udrset("RSIZET", 0.015)      # ���󥿡���å�����ʸ���Υ�����
#DCL::swlset('LDUMP', true)        # ����ץ��᡼�������

DCL::gropn(iws)

for j in 0..20

  varname = "comp"
  Comp = GPhys::NetCDF_IO.open(filename,varname)
  Comp = Comp[0..-1,0..-1,j]

  DCL::grfrm
  DCL::grswnd(xmin,xmax,ymin,ymax)
  DCL::grsvpt(vxmin,vxmax,vymin,vymax)
  DCL::grstrn(1)
  DCL::grstrf

  DCL::sglset("LCLIP", true )
  DCL::uwsgxa(Comp.coord(0).val)
  DCL::uwsgya(Comp.coord(1).val)
  DCL::ussttl(x_title, x_unit, y_title, y_unit)
  DCL::ueitlv
  DCL::uegtla(0.0, 1.0, 0 )
  DCL::uetonf(Comp.val)
  DCL::usdaxs
  DCL::udgcla(0.0, 1.0, 0 )
  DCL::udcntz(Comp.val)
  DCL::sglset("LCLIP", false )
  DCL::uzrset("ROFFXT", 0.06)
  title = "Salinity [1]"
  DCL::uxsttl("t", title, 0 )
  DCL::Util::color_bar($cbar_conf)


end

DCL::grcls

