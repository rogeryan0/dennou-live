#!/usr/bin/env ruby

=begin

title   : shallow_eqbeta_nonlinear.rb
history : 2004/07/04 YAMADA, YUKIKO

usage   : ruby shallow_eqbeta_nonlinear.rb

pstoeps : $ dclpsrmcm dcl.ps > tmp.ps
          $ eps2eps tmp.ps thermal_responce.eps
          $ rm tmp.ps dcl.ps
=end 


# ----------------------------------------------
# ɬ�פʥ饤�֥��, �⥸�塼����ɤ߹���
require "numru/ggraph"
require "numru/dcl"
require "colorbar" 
include NumRu

# ----------------------------------------------
# �ե�����̾, �ѿ����ɤ߹���

filename = "thermal_responce.nc"
varname = "height"
gphys = GPhys::NetCDF_IO.open(filename,varname)


# ----------------------------------------------
# �볫��

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(-iws)


# -----------------------------------------------------
# �����������γƼ�����

# �ӥ塼�ݡ���
vxmin = 0.1   ; vxmax = 0.9
vymin = 0.045 ; vymax = 0.23
GGraph.set_fig('viewport'=>[vxmin,vxmax,vymin,vymax])

# ���Υ����ȥ�ñ�̤�ɽ�����ʤ�
GGraph.set_axes('xtitle'=>'','ytitle'=>'','xunits'=>'','yunits'=>'')


DCL::sldiv('T', 1, 6)          # ���� 6 ʬ��
DCL::sglset("LCORNER", false ) # �����ʥޡ����ϤĤ��ʤ�
DCL::sglset("LCNTL", false )   # ��������С���ɽ��
DCL::udlset("LMSG", false )    # ����դβ����ޡ�����˥�å�������񤫤ʤ� 
DCL::sglset('LFULL', true)     # ������ɽ��
DCL.uzfact(0.9)                # ��ɸ����ʸ���󥵥����������
DCL.sgpset('lfprop',true)      # �ץ�ݡ�����ʥ�ե���Ȥ�Ȥ�
DCL.uzpset('labelxb',false)    # x ����ɽ�����ʤ�


# -----------------------------------------------------
# ���顼, �ȡ��������

levels   = NArray[-1,-0.012,-0.006,-0.004,0.0,0.004,0.006,1]
patterns = NArray[30999, 35999, 40999, 55999, 70999, 75999, 85999]

$tone_hash = { 'patterns'=> patterns, 'levels'=>levels,'title'=>nil }
$cont_hash = { 'int' => 0.002, 'title'=>nil }

# ���顼�С�������
$cbar_conf = {
  "vx0"=>0.1,           # ���顼�С��κ����Ѥ� x ��ɸ
  "vy0"=>0.15,          # ���顼�С��κ����Ѥ� y ��ɸ
  "vxlength"=>0.7,      # ���顼�С��� ����Ĺ��
  "vylength"=>0.03,     # ���顼�С��� �Ĥ�Ĺ��
  "levels"=>levels, 
  "colors"=>patterns,
  "eqlev"=>true, 
  "nobound"=>0, 
  "tick1"=>7,"tick2"=>1,
  "label_size"=>0.013
  }


# -----------------------------------------------------
# ���������ᥤ��

# �����ȥ�
DCL::grfrm
DCL::sgtxzv(0.5,0.1,'Height', 1.15*DCL.uzpget('rsizec2'),0,0,3)


# �¸���̿� 4 ��
4.times{ |num|
  DCL.uzpset('labelxb',true) if num == 3   # �ǲ��ʤΤ� x ��ɽ�� 
  GGraph.tone( gphys[true,true,(num+1)*10],true, $tone_hash) 
  GGraph.contour( gphys[true,true,(num+1)*10], false, $cont_hash)
}

# ���顼�С�
DCL::grfrm
DCL::Util::color_bar($cbar_conf)


# �����դ�
DCL::grcls






