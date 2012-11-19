=begin
= GPhys::EP_Flux ���塼�ȥꥢ��

�͸�����, ��Ƿ����

��������: 2004 ǯ 9 ��

�ǽ�����: 2005 ǯ 02 �� 28 ��


== �ܼ�

((:<ol class="contents">:))
<<< index.hindex.rd
((:</ol>:))

==�Ϥ����

GPhys::EP_Flux ����®����Ӳ��٤� GPhys ���֥������Ȥ���Ҹ��̾����������
Elliassen-Palm Flux (EP Flux) �䤽��ȯ����׻�����ؿ��򽸤᤿�⥸��
����Ǥ�.
�����Ǥ� GPhys::EP_Flux ���Ѥ����Ѥ�������, ������ˡ�䤽�����٤β����Ԥ��ޤ�.
�ܥ��塼�ȥꥢ��Ǥ� GPhys. ver 0.3.5 ���Ѥ��뤳�Ȥ�����Ȥ����ä�ʤ�ޤ�.

== ��������

GPhys::EP_Flux ���Ѥ������������ϥѥå�������°�� doc/math-doc/((<document.pdf|URL:math-doc/document.pdf>))
�� doc/math-doc/document/((<document.html|URL:document/document.html>)) �򻲾Ȳ�����. (html �Ǥ�����
���줷���Ǥ���, ������������.)


==����
GPhys::EP_Flux �����Ѥ���ˤ� GPhys �����ѤǤ���ɬ�פ�����ޤ�. 2005/02/12 ���ߤκǿ��Ǥ� ver.0.3.5 �Ǥ�.
GPhys �Υ��󥹥ȡ�����ˡ��((<GPhys Install|URL:http://dennou-k.gfd-dennou.org/arch/ruby/products/gphys/>))�򻲾Ȥ�������.
( GPhys::EP_Flux �� GPhys ver 0.3.4 ��� GPhys ���Τ˼����ޤ�ޤ���. )


==�ܥ��塼�ȥꥢ����Ѥ���ǡ���

�ܥ��塼�ȥꥢ��Ǥϥƥ��ȥǡ����Ȥ���, NCEP �Ʋ��Ϥˤ�� 2001 ǯ 4 �������
����®����Ӳ��٤Υǡ�����Ȥ��ޤ�.
((<������|URL:./testdata.tar.gz>))
�����������κ���ѥǥ��쥯�ȥ�˥���������ɤ���Ÿ�����Ƥ�������.
���塼�ȥꥢ��Υ�˥塼��Ÿ�����ƽ��褿�ǥ��쥯�ȥ�(testdata/)�������ȥǥ�
�쥯�ȥ�ˤ���Ȥ��Ƽ¹Ԥ����ΤȤ��ޤ�.

�ޤ��ϥƥ��ȥǡ����γ��פ򸫤Ƥߤޤ��礦. �ƥ��ȥǡ����ե������ 4 �Ĥ���,
���줾��������®(UWND_NCEP.nc), ������®(VWND_NCEP.nc), ��ľ��®(OMEGA_NCEP.nc)
, ����(TEMP_NCEP.nc) �Υǡ����Ǥ�. ������, ��ľ��®�ϰ���®�٤� Pa/s ��ñ�̤�
���Ƥ��ޤ�. �����Ǥ� NetCDF��°�Υ��ޥ�� ncdump ���Ѥ��� UWND_NCEP.nc ��
��Ȥ򸫤Ƥߤޤ��礦.

   % ncdump -c UWND_NCEP.nc
                           
   netcdf UWND_NCEP {
   dimensions:
           lon = 72 ;
           lat = 37 ;
           level = 12 ;
           time = 120 ;
   variables:
           float lon(lon) ;
                   lon:units = "degrees_east" ;
                   lon:long_name = "Longitude" ;
                   lon:actual_range = 0.f, 357.5f ;
           float lat(lat) ;
                   lat:units = "degrees_north" ;
                   lat:actual_range = 90.f, -90.f ;
                   lat:long_name = "Latitude" ;
           float level(level) ;
                   level:units = "mb" ;
                   level:actual_range = 1000.f, 10.f ;
                   level:long_name = "Level" ;
                   level:positive = "down" ;
                   level:GRIB_id = 100s ;
                   level:GRIB_name = "hPa" ;
           double time(time) ;
                   time:units = "hours since 1-1-1 00:00:0.0" ;
                   time:long_name = "Time" ;
                   time:actual_range = 17531688., 17540442. ;
                   time:delta_t = "0000-00-00 06:00:00" ;
           short uwnd(time, level, lat, lon) ;
                   uwnd:long_name = "4xDaily U-wind" ;
                   uwnd:valid_range = -125.f, 160.f ;
                   uwnd:actual_range = -90.5f, 128.6f ;
                   uwnd:units = "m/s" ;
                   uwnd:add_offset = 202.66f ;
                   uwnd:scale_factor = 0.01f ;
   
   ..��ά..
                     
   // global attributes:
                   :history = "2004-08-25 20:31:49 JST daktu32> NumRu::GPhys::NetCDF_IO.write uwnd" ;
                   :title = "4xDaily U-wind, NMC reanalysis (2001-04)" ;
   data:
     
    lon = 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 
       90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 
       165, 170, 175, 180, 185, 190, 195, 200, 205, 210, 215, 220, 225, 230, 
       235, 240, 245, 250, 255, 260, 265, 270, 275, 280, 285, 290, 295, 300, 
       305, 310, 315, 320, 325, 330, 335, 340, 345, 350, 355 ;
     
    lat = 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 25, 20, 15, 10, 5, 
       0, -5, -10, -15, -20, -25, -30, -35, -40, -45, -50, -55, -60, -65, -70, 
       -75, -80, -85, -90 ;
     
    level = 1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100 ;
     
    time = 17533848, 17533854, 17533860, 17533866, 17533872, 17533878, 17533884, 
       17533890, 17533896, 17533902, 17533908, 17533914, 17533920, 17533926,
     
   ..��ά..
       
       17534562 ;
   }
      
              
���� lon, lat�ϰ��٤���ӷ���, level �Ϲ���(����), time ��
����(���� 1/1/1 00:00:00 �����Ȥ�������)�򤽤줾��ɽ���ޤ�. ���ꥸ�ʥ��
NCEP �ǡ����Ͽ�ʿ�� 2.5 �٤�ʬ��ǽ�Ǥ���, �����Ǥ��Ƥ����ƥǡ����������򲡤�
�Ƥ���ޤ�. ����ʬ��ǽ�� 6 ������Ǥ�. 

==�ޤ��ϻȤäƤߤ褦

�ʲ��Υץ�������¹Ԥ���, EP_Flux �Ȥ���ȯ����׻����Ƥߤޤ�
�礦. ���Υץ������ϥƥ��ȥǡ������ɤ߹���ǻҸ��̾�� EP_Flux ��׻���,
����ˤ���ȯ�������, netCDF �ե�����(epflx_NCEP.nc)�Ȥ�����¸����
��ΤǤ�. �ʹ��Ǥ꤬�ʤ��¤�, �ܥ��塼�ȥꥢ����Ѥ���ץ����������ƥѥå�
��������°����ǥ�ץ���������Ѥ��ޤ�. �ʤ����ֹ���������ؤΤ���ˤĤ���
��Ƥ���, �ºݤϤ���ޤ���.

((<demo_NCEP_1.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/ep_flux/demo/demo_NCEP_1.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01  require 'numru/gphys'
02  require 'numru/gphys/ep_flux'
03  include NumRu
04
05  datadir = "./testdata/"
06  gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
07  gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
08  gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
09  gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
10
11 epflx_y, epflx_z, v_rmean, w_rmean, gp_lat, = ary =
12           GPhys::EP_Flux::ep_full_sphere(gp_u, gp_v, gp_omega, gp_t, true)
13 gp_lat.rename('phi')
14
15 ary.each{|gp|                                  #  This part will not 
16   gp.data.att_names.each{|nm|                  #  be needed in future.
17     gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
18   }                                            #  needed if the valid
19 }                                              #  range is wide enough)
20
21 epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
22 strm_rmean = GPhys::EP_Flux::strm_rmean(v_rmean)
23
24 ofile = NetCDF.create("epflx_NCEP.nc")
25
26 ary.each{|gp|                     
27   GPhys::IO.write(ofile, gp)      
28 }                                 
29 GPhys::IO.write(ofile, epflx_div)
30 GPhys::IO.write(ofile, strm_rmean)
31
32 ofile.close
((:</textarea>:))

1-3 ���ܤϤ�����֤���ˡ����ʬ�Ǥ�. 1-2 ���ܤ�GPhys::EP_Flux �����Ѥ��뤿��
��ɬ�פʥ饤�֥����ɤ߹���Ǥ��ޤ�. 'numru/gphys' �� 'numru/gphys/ep_flux'
�������Ǥ�ƤФ��Τ�, 2 ���ܤ����Ǥ� OK �Ǥ�. 3 ���ܤȤ��뤳�Ȥ� NumRu �ʲ�
��̾�����֤�Ǽ����Ƥ���⥸�塼���, NumRu:: ���ά���ƸƤ֤��Ȥ��Ǥ��ޤ�. 

6-9 ���ܤǥƥ��ȥǡ����� GPhys ���֥������ȤȤ��Ƴ����Ƥ��ޤ�. ������,
�ºݤ˥�����ɤ߹��ޤ��Τϱ黻��Ԥ���ʬ(13-14 ����)��, ���λ����Ǥϻ��ꤵ
�줿�ե���������ѿ���ؤ��Ƥ�������Ǥ�. �����Ǥ� gp_u, gp_v, gp_omega, gp_t
���줾���������®, ������®, ��ľ��, ������������Ƥ��Ƥ��ޤ�.


11-12 ���ܤ� GPhys::EP_Flux::ep_full_sphere ��Ƥ�� EP Flux ��׻����Ƥ��ޤ�.
ep_full_sphere �ϵ��̾�� EP Flux �������Ѥ��ʤ��ե륻�åȤμ��򸵤˷׻�
����ؿ��Ǥ�. 5 ���ܤΰ����ϵ����Υǡ������ֲ��١פǤ��뤳�Ȥ��̣���Ƥ��ޤ�
. ep_full_sphere �ϲ��٤�����˲��̤�����ˤȤ뤳�Ȥ���ǽ�ʤΤǤ�.
���ξ�� false �Ȥ��ޤ�. ���ʤ�����ΰ�����Ϳ����줿�ǡ�����
���٤ʤΤ����̤ʤΤ�����ꤷ�Ƥ���櫓�Ǥ�. ��ά�������,
�ǥե���Ȥ��ͤ� true (����) �ȤʤäƤ��ޤ�. �ޤ��嵭�ǤϾ�ά����Ƥ��ޤ���,
6 ���ܤΰ����ˤϳ� GPhys ���֥������Ȥμ������֤���ꤹ����������Ϥ��ޤ�.
���ټ��� 0 , ���ټ��� 1, ��ľ���� 2 ��ɽ�����������, �ǥե���Ȥ� [0, 1, 2]
�ˤʤ�ޤ�. ���ʤ�� 0 �����ܤ˷���, 1 �����ܤ˰���, 2 �����ܤ˱�ľ����
���äƤ���Ȼפ��櫓�Ǥ�. 

11 ���ܤˤĤ��Ƥ⤦�����ܤ������⤷�ޤ�. ep_full_sphere �Ϲ�� 11 �ĤΥ��֥���
���Ȥ�����ͤȤ��ƻ����ޤ�(�ġ��Υ��֥������Ȥξܺ٤ϸ�Ҥ��ޤ�). 11 ���ܤǤ�
ruby ��¿�����������Ѥ���, ��������ƤΥ��֥������Ȥ� ary �Ȥ����������������
��Ʊ������Ƭ�� 5 �ĤΥ��֥������Ȥ򤽤줾�� epflx_y, epflx_z, v_rmean, w_rmean,
gp_lat �Ȥ����ѿ����������Ƥ��ޤ�. ���դ� , �ǽ���äƤ��뤳�Ȥ����դ��Ƥ�������.
����ˤ�� ep_full_sphere �� 6 ���ܰʹߤ�����ͤ��ΤƤ��ޤ�. �⤷�� "," ��ȴ��
���� gp_lat ������Ȥ��� 5 - 11 ���ܤΥ��֥������Ȥ���Ǽ����뤳�Ȥˤʤ�ޤ�.


13 ���ܤǤ� gp_lat �Υǡ�����ʬ��̾���� "phi" ���֤������Ƥ��ޤ�(�ǥե���Ȥ� "lat").
��ҤΤȤ���, GPhys ���֥������Ȥ��ѿ���̾����°���Ȥ��ƻ��Ĥ��Ȥ��Ǥ���ΤǤ���,
�ѹ����������Ϥ��Τ褦�ˤ���ФǤ��ޤ�.

15-19 ���ܤϥ��ƥ졼�����Ѥ�������ͤ����ƤΥǡ�����ʬ��°�� "valid_hoge" ��
������Ƥ��ޤ�. "valid_hoge" ��ͭ���ϰϤ˴ؤ���°����, ���ꥸ�ʥ�Υǡ�����
���ä�°���Ǥ�. GPhys ��°���ͤ��ۤ˻��ꤷ�ʤ��¤�, �ǽ���ɤߤ�����ǡ�����
�Ф��Ƥ���Τ�, �黻��������Ѳ�������(�����ͭ���ϰϤ�����ˤ�����ޤ���)
�����Թ礬�����Ƥ��ޤ��ޤ�. ���˹����ϰϤ�Ϳ�����Ƥ��ʤ��¤�, �����ͤ�
�ѹ�����ʤ��ƤϤʤ�ޤ���. ����Ū�� GPhys ���Ȥ�����������н褷�ޤ�.

21 ���ܤ� GPhys::EP_Flux::div_sphere ��Ƥ�� EP Flux ��ȯ����׻����Ƥ��ޤ�.
div_sphere �ϻҸ��̾�� y-z ʿ�̤ˤ�����ȯ����׻�����᥽�åɤ�, ������ EP_Flux
�ʳ���ʪ���̤ˤ����ѤǤ��ޤ�. 22 ���ܤǻĺ��۴Ĥ��������������ή���ؿ���
���㤹���̤���ޤ�. 


24-32 ���ܤ�, �׻���̤� NetCDF �ե�����Ȥ�����¸���ޤ�. 23 ���ܤ���¸�ե�����
�������, 25-28 ���ܤǸġ��� GPhys ���֥������Ȥ�񤭹��ߤޤ�. GPhys::IO.write
�� GPhys ���֥������Ȥ� NetCDF �˽񤭹������ѤΥ᥽�åɤǤ�. �Ǹ�� 30 ���ܤ�
��¸�ե�����򥯥��������Ƥ��ޤ�. ���ιԤ�̵�����������¸��λ���뤳�Ȥ��Ǥ�
�ʤ��Τ�, ˺�줺��.

����Ǥϼ¹Ԥ��ƤߤƤ�������. �׻�����λ����ޤǤˤ�������֤����ԤδĶ�
(CPU:Celeron 1.1GHz, RAM: 256 MB) �� 1 ʬ���٤Ǥ�. �������¹Ԥ��줿�ʤ��
�����ȥǥ��쥯�ȥ��, 'epflx_NCEP.rb' �Ȥ����ե����뤬���褿���ȤȻפ��ޤ�.
���褿�ե�����Υإå��� ncdump ���Ѥ��ƽ��Ϥ��Ƥߤޤ��礦. �ʲ��Τ褦��
�ʤ�Ȼפ��ޤ�(������ά���Ƥ��ޤ�).

  % ruby demo_NCEP_1.rb
  ..���Ф��Ԥ�..
  % ncdump -h epflx_NCEP.nc

    netcdf epflx_NCEP {
    dimensions:
            lat = 37 ;
            level = 12 ;
            time = 120 ;
    variables:
             ..��ά..
            float epflx_y(time, level, lat) ;
             epflx_y:long_name = "EP flux y component" ;
             epflx_y:units = "Pascal.kg-1 m3" ;
             ..��ά..
            float v_rmean(time, level, lat) ;
             v_rmean:long_name = "EP flux z component" ;
             v_rmean:units = "m/s" ;
             ..��ά..
  // global attributes:
                  :history = "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write epflx_y\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write epflx_z\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write v_rmean\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write w_rmean\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write phi\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write z\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write uwnd\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write temp\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write uv_dash\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write vt_dash\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write uw_dash\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write dtemp_dz\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write epflx_div" ;
  }

global attribute �˽񤫤�Ƥ��� history °�������� GPhys ����ưŪ���ղä��Ƥ�
����ΤǤ�. ��¸�����ѿ����Ƥ��Ф��� history ���Ǥ���Ƥ��ޤ�. ep_full_sphere
�����Ϥ����ѿ��ϰʲ��ΤȤ���Ǥ�.ep_full_sphere ������ͤν��֤���󤷤ޤ�.

  epflx_y   : EP Flux �ΰ�����ʬ
  epflx_z   : EP Flux �ι�����ʬ
  v_rmean   : �ĺ�����ή®
  w_rmean   : �ĺ���ľή®
  phi       : ����(ñ�̤� radian)
  z         : ����(ñ�̤� m)
  uwnd      : �Ӿ�ʿ��������®(�ѿ�̾�����ϥǡ����˰�¸)
  temp      : �Ӿ�ʿ�ѵ���(�ѿ�̾�����ϥǡ����˰�¸)
  uv_dash   : �Ӿ�ʿ�Ѿ���ư�̥ե�å���
  vt_dash   : �Ӿ�ʿ�Ѿ���Ǯ�ե�å���
  dtemp_dz  : �Ӿ�ʿ�ѵ�����ľ����

demo_NCEP_1.rb �ǤϤ�������Ƥ�ե��������¸���뤳�Ȥˤ��Ƥ��ޤ�. �嵭���ѿ����Ƥ�
��¸����Ƥ��뤳�Ȥ��إå��˵��Ҥ���Ƥ��ޤ�.

GPhys::EP_Flux �δ���Ū�ʻȤ����ϰʾ�Ǥ�. �׻���Ψ�β����ʤɤ�ͤ��ʤ����, �嵭��
���������������� EP_Flux ��׻��Ǥ��ޤ�. 
�����Ǥϥƥ��ȥǡ����Ȥ���netCDF ������ NCEP �Ʋ��ϥǡ������Ѥ��ޤ�����, GPhys ���б����������
�ǡ����Ǥ���н����Ǥ��ޤ�. ���ԤǤ� GRADS �Υǡ����Ǥ� OK �Ǥ�. �ʲ��ˤ���������Ƥ�ޤȤ�ޤ�.

((:<b>:))
((:<font color=rgb(87,153,51)>:))

�ޤȤ� - �ޤ��ϻȤäƤߤ褦 

(1) EP Flux ��׻�����ˤ� require "numru/gphys/ep_flux" ����
    GPhys::EP_Flux::ep_full_sphere ��Ƥ�
(2) ɬ�פ� GPhys ���֥������Ȥ�������®, ������®, ��ľ��®, ����(����)
(3) 5 ���ܤΰ����ϲ��٤ʤ� true, ���̤ʤ� false. �ǥե���Ȥ� true.
(4) ���ϥ��֥������Ȥμ�������Ʊ��. �ǥե���Ȥ�["����", "����", "��ľ��"]�ν�.
(5) ep_full_sphere ������ͤν��֤�����
    * ���
      epflx_y, epflx_z, v_rmean, w_rmean, phi, z, uwnd, theta, uv_dash,
      vt_dash, uw_dash, dtheta_dz

((:</font>:))
((:</b>:))
      
==����ǡ������н�ˡ

demo_NCEP_1.rb �Υ����ɤǤϸ��Υǡ������ΰ��쵤�˥������ɤ߹����
����׻���Ԥ�, ��λ��˷�̤�ե�����˽��Ϥ��Ƥ��ޤ�. ����
��ˡ�ǤϤ�������Υ����ΰ�򿩤��Ĥ֤�¾, ���ƤγʻҤˤ�����׻�����
λ����ޤǷ׻���̤����ˤ���Ƥ������Ȥˤʤ�, ��Ψ�������Ǥ�.
�ޤ�, �����ѿ���������׽񤭽��äƤ���, ���ν��Ϥ˼��ݤ���Ȥ������,
GPhys::IO �β���ư���Ƥ��� NetCDF Ū�ˤ�΢�ǥե�����������ԡ��Ⱥ��ľ��
�������ޤ�. GPhys �ǤϤ������¤���褦, ���������ȼ��˽��Ϥ�Хåե���
�󥰤���褦�ˤʤäƤ�ΤǤ���, ���ޤ��礭���ʤ�ȥ��꡼�������ޤ���
��, �����ʤ��Хåե�����ե�å��夹���Ĥޤ������� NetCDF �˽�Ф��Ȥ�
�����Ȥ򤷤ޤ�. �����ʤ�Ȥ��θ�Ͼ嵭�������ԡ��Ⱥ��ľ���������뤳�Ȥ�
�ʤ�ޤ�.

�ǤϤɤ�����и�ΨŪ�ʤΤǤ��礦��? ����Ͻ�������ǡ�����Ŭ����ʬ�䤷,
�༡�ե�����˽��Ϥ���褦�ˤ���Ф褤�Ǥ�. 
����Υƥ��ȥǡ����ϻ��������� 120 �Ĥγʻ�������äƤ��ޤ��Τ�, ���ּ��˱�äƥ롼�פ�󤹤��ɤ��Ǥ��礦.
��������з׻���ɬ�פʥǡ�������Ŭ���������ɤ߽Ф�, �׻������꼡��
�ե�����˽񤭽Ф�. �����Ƽ��Υǡ������ɤ߹���...�Ȥ����褦�ˤ��Ƥ�����
���������ˤʤ�ޤ�. ����ǤϾ嵭�θ�Ψ���� GPhys::IO �Υ��ƥ졼����
�Ѥ��Ƽ���������ˡ��Ҳ𤷤ޤ�. �ʲ��Υץ�����������������.

((<demo_NCEP_2.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/ep_flux/demo/demo_NCEP_2.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01  require 'numru/gphys'
02  require 'numru/gphys/ep_flux'
03  include NumRu
04
05  datadir = "./testdata/"
06  gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
07  gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
08  gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
09  gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
10
11 ofile = NetCDF.create("epflx_NCEP.nc")
12
13 nt = gp_u.shape[-1]
14 i = 0
15 GPhys::IO.each_along_dims_write([gp_u, gp_v, gp_omega, gp_t], ofile, -1){
16   |u, v, omega, t|
17   i += 1
18   print "processing #{i} / #{nt} ..\n" if (i % (nt/20+1))==1
19
20   epflx_y, epflx_z, v_rmean, w_rmean, gp_lat, gp_z, u_mean, theta_mean,
21         uv_dash, vt_dash, uw_dash, dtheta_dz = ary =
22                 GPhys::EP_Flux::ep_full_sphere(u, v, omega, t, true)
23
24   ary.each{|gp|                                  #  This part will not   
25     gp.data.att_names.each{|nm|                  #  be needed in future. 
26       gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not 
27     }                                            #  needed if the valid  
28   }                                              #  range is wide enough)
29
30   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
31   strm_rmean = GPhys::EP_Flux::strm_rmean(v_rmean)
32
33   if i==1    # time independent => write only once                     
34     gp_lat.rename('phi')                                               
35     GPhys::IO.write(ofile, gp_lat)                                     
36     GPhys::IO.write(ofile, gp_z)                                       
37   end                                                                  
38   [ epflx_y, epflx_z, v_rmean, w_rmean, strm_rmean, epflx_div, u_mean, theta_mean, 
39     uv_dash, vt_dash, uw_dash, dtheta_dz ]                             
40 }                                                                      
41                                                                        
42 ofile.close                                                            
43                                                                        
((:</textarea>:))

���Υץ�����������Υǥ�ץ������ (demo_NCEP_1.rb) Ʊ�ͤ�, �ƥ��ȥǡ�������
�Ҹ��̾�� EP Flux ¾��׻���, nc �ե��������¸�����ΤǤ�. ������, 13 ���ܰʹ�
���礭���ۤʤ�ޤ�. ��˽Ҥ٤��׻��θ�Ψ����ޤ뤿��, GPhys �Υ��ƥ졼��������
���Ƥ��ޤ�. 

1-11 ���ܤ� demo1 ��Ʊ�����饤�֥�ꤪ��ӥǡ������ɤ߹��ߤ���¸�ե�����Υ����ץ��
�ԤäƤ��ޤ�.

13-14 ���ܤϸ�ҤΥ��ƥ졼����ʬ�ν����οʹ��ٹ礤����Ϥ��뤿����������Ǥ�.
13 ���� (nt = gp_u.shape[-1]) �� gp_u �ΰ��ֺǸ�μ����Υ���åɿ����֤��ޤ�. ���Υƥ���
�ǡ����Ǥϻ��� time �Υ���åɿ� 120 �� nt ���������ޤ�.

15-40 ���ܤ� demo1 �� 11-28 ���ܤ򥤥ƥ졼�����Ѥ��Ƽ������Ƥ��ޤ�.
GPhys::IO.each_along_dims_write �����Υ��ƥ졼���ؿ��Ǥ�.
�ܺ٤�
((<GPhys::NetCDF_IO �Υ�ե���󥹥ޥ˥奢��|URL:http://dennou-k.gfd-dennou.org/arch/ruby/products/gphys/doc/gphys_netcdf_io.html#label:6>))
����������.   
�ʲ�, ������ʬ���ڤ�Ф��Ʋ��⤷�ޤ�. 

      15 GPhys::IO.each_along_dims_write([gp_u, gp_v, gp_omega, gp_t], ofile, -1){
      16   |u, v, omega, t|
      ...
      20   epflx_y, epflx_z, v_rmean, w_rmean, gp_lat, gp_z, u_mean, theta_mean,
      21         uv_dash, vt_dash, uw_dash, dtheta_dz = ary =
      22                 GPhys::EP_Flux::ep_full_sphere(u, v, omega, t, true)
      ...
      38   [ epflx_y, epflx_z, v_rmean, w_rmean, strm_vrmean, epflx_div, u_mean, theta_mean,
      39     uv_dash, vt_dash, uw_dash, dtheta_dz ]
      40 }


15 ���ܤ���������. GPhys::IO.each_along_dims_write �������� (�����Ǥ� [gp_u, ..., gp_t]) �ˤ�
���ƥ졼����ǽ������� GPhys ���֥������Ȥ����Ǥ˻�������, �⤷���� GPhys
���֥������ȼ��Ȥ��Ϥ��ޤ�. �� 2 �����ˤ���¸�оݤΥե����륪�֥������Ȥ��Ϥ��ޤ�.
�� 3 �����ʹߤϥ롼�פ�󤹼�����̾���⤷���Ͽ����ǻ��ꤷ�ޤ�. �����Ǥϰ��ֺǸ��
�����Ǥ��� time �������Ƥޤ�. 

16 ���ܤǤ� gp_u, gp_v, gp_omega, gp_t ����ּ��˱�äƥ��饤���������֥������Ȥ�
���줾�� u, v, omega, t �Ȥ��ƥ��ƥ졼������Ϥ��Ƥ��ޤ�. 

20-22 ���ܤ� demo1 Ʊ�� EP_Flux::ep_full_sphere ��ƤӽФ��Ƴ��ѿ��η׻���ԤäƤ��ޤ�.
ͣ��ۤʤ�Τ�, Ϳ�������������ּ��˱�äƥ��饤�����줿���֥������ȤǤ������Ǥ�.

38-39 ���ܤ�, ��¸�оݥե����� ofile ����¸�����ѿ������Ǥ˴ޤ��������������
���ƥ졼���Υ֥��å����Ĥ��ޤ�. ���֤˰�¸�����ѿ�, ���ʤ�����ּ�������ѿ�
�Τߵ��ܤ��Ƥ��뤳�Ȥ����դ��Ƥ�������. (���֤˰�¸���ʤ��ѿ�, �����Ǥ� gp_lat ��, ��
�롼����˾�񤭤����̵�̤ʤΤ�, 33 - 37 ���ܤΤ褦�� 1 ������ե�����˽񤭹�
��褦���פ��Ƥ��ޤ�.) 42 ���ܤǥե�����Υ���������ԤäƤ��ޤ�. 

����Ǥϥץ�������¹Ԥ��Ƥߤޤ��礦. �¹����ɸ����Ϥˤϰʲ��Τ褦�ˤʤ��
�פ��ޤ�. 18 ���ܤˤƿʹԾ�����롼�פβ�����Ѥ���ɽ�����Ƥ��뤿��Ǥ�. ������
ʬ�Ϸ褷��ɬ�ܤǤϤ���ޤ���, �׻��˻��֤Τ�����ץ������ˤ����Ƥϼ�������
���Ǥ���.

  % ruby demo_NCEP_2.rb

    processing 7 / 120 ..
    processing 14 / 120 ..
    ...
    processing 120 / 120 ..

����˽�λ������ ncdump ���Ѥ��ƥإå��������Ƥߤޤ��礦. demo_NCEP_1.rb ������
���� nc �ե������Ʊ���Ǥ��뤳�Ȥ��狼��ޤ� (�ѿ��ν��֤ϰ㤦���⤷��ޤ���
, �����Ϥ�����������).

�ʾ�ǥ��ƥ졼�����Ѥ����ץ�������Ҳ𤷤ޤ���. ���ƥ졼����Ȥ���, �����
�����¾, �ġ����ѿ��ϻ��ּ��˱�äƥ��饤��������Τ�����˽������Ƥ������Ȥ�
�ʤ�Τ�, ���ϸ�Ψ�β������ޤ�ޤ�. ΢���֤���, ����ʥǡ����򰷤��Ȥ���, ����
�ڤ�ˤ��ȥ��꡼���������Ʊ���˽��ϸ�Ψ�Ⲽ�����ǽ��������Τ�, ���ƥ�
��������Ȥ��Ȥ������ȤǤ�.

((:<b>:))
((:<font color=rgb(87,153,51)>:))

�ޤȤ� - ����ǡ����ؤ��н�ˡ

(1) ����ǡ����ϥ��ƥ졼����Ȥäƽ���
    * �Ȥ�����GPhys::NetCDF_IO.each_along_dims_write(gphyses, ofile, dims)
(2) GPhys::EP_Flux ���Ѥ�����, ���ּ�����ĥǡ�����ͭ��
  
((:</font>:))
((:</b>:))

    
==��ñ��������

����ǤϤ��褤�� GPhys::EP_Flux �ǽ��Ϥ����ʪ���̤�޼����Ƥߤޤ��礦. EP Flux ��
�������ˤϥ٥��ȥ�ޤ��٥��ȤǤ�. �����Ǥ� EP Flux ��٥��ȥ�, �����С������󥹤�
���󥿡�����ӥȡ���ǽŤͤ�������ץ�������Ҳ𤷤ޤ�. ���ޤǤΥץ���������Ĺ
���Ǥ���, 01-37 ���ܤϤ���ˡ����ӥǡ������ɤ߹�����ʬ��, ���פʤΤϸ�Ⱦ��ʬ�Ǥ��Τ�
���¿�������.

((<demo_NCEP_3.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/ep_flux/demo/demo_NCEP_3.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01  require 'numru/gphys'
02  require 'numru/gphys/ep_flux'
03  require 'numru/gphys/ggraph_on_merdional_section'
04  include NumRu
05 
06  epflx_fnm = './epflx_NCEP.nc'
07 
08  if File::exist?(epflx_fnm)
09 
10   epflx_y =   GPhys::IO.open(epflx_fnm,  'epflx_y')
11   epflx_z =   GPhys::IO.open(epflx_fnm,  'epflx_z')
12   epflx_div = GPhys::IO.open(epflx_fnm,  'epflx_div')
13   v_rmean =   GPhys::IO.open(epflx_fnm,  'v_rmean')
14   w_rmean =   GPhys::IO.open(epflx_fnm,  'w_rmean')
15   strm_rmean =   GPhys::IO.open(epflx_fnm,  'strm_rmean')
16
17 else
18
19   datadir = "./testdata/"
20   gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
21   gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
22   gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
23   gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
24
25   epflx_y, epflx_z, v_rmean, w_rmean= ary =
26     GPhys::EP_Flux::ep_full_sphere(gp_u, gp_v, gp_omega, gp_t, true)
27
28   ary.each{|gp|                                  #  This part will not
29     gp.data.att_names.each{|nm|                  #  be needed in future.
30       gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
31     }                                            #  needed if the valid
32   }                                              #  range is wide enough)
33
34   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
35   strm_rmean = GPhys::EP_Flux::strm_rmean(v_rmean)
36
37 end
38
39
40 DCL.gropn(1)
41 DCL::sglset('LFULL', true)                       # use full area
42 DCL::slrat(1.0, 0.85)                            # set aspect ratio of drwable area
43 DCL.sgpset('lcntl', false)                       # don't rede control character.
44 DCL.sgpset('lfprop',true)                        # use prportional font
45 DCL.uzfact(0.6)                                  # set character size
46
47 ## show vector (Fy, Fz)
48 fy = epflx_y.mean('time')
49 fz = epflx_z.mean('time')
50 epdiv = epflx_div.mean('time')
51
52 GGraph::set_fig('view'=>[0.15, 0.85, 0.25, 0.55])
53 GGraph::set_fig('itr'=>2)                        # contour && tone only
54 GGraph::tone(epdiv)                              # tone
55 GGraph::contour(epdiv,false)                           # contour
56 GGraph::vector_on_merdional_section(fy, fz, false,
57    'fact'=>3.0,'xintv'=>1,'unit'=>true, 'annot'=>false
58                                     )
59
60 ## show residual mean merdional circulation (cut nearly surface and Antarctica)
61 vrm =  v_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
62 wrm =  w_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
63 strm = strm_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
64 GGraph::contour(strm, true, 'nlev'=>25)
65 GGraph::vector_on_merdional_section(vrm, wrm, false,
66                         'fact'=>2.0,'xintv'=>1,'unit'=>true, 'annot'=>false
67                                     )
68 DCL.grcls
((:</textarea>:))

�¹Ԥ���Ȥޤ��ʲ��γ�(EP-Flux(�٥��ȥ�)�Ȥ���ȯ��(�ȡ���))�����̤����褵��ޤ�.

((:<center><A HREF="./epflx.png"><IMG SRC="epflx.png" HEIGHT=300 WIDTH=400></A></center>:))

���ڡ����������Ǹ����뤫���򥯥�å�����ȼ��γ�(�ĺ��۴�(�٥��ȥ�)�ȼ���ή���ؿ�(���󥿡�))
�����Ϥ���ޤ�. �ʤ�, ����ή���ؿ��� EP_Flux::strm_rmean �Ȥ����᥽�åɤˤ�äƷ׻��������
�Ǥ�.

((:<center><A HREF="./residual.png"><IMG SRC="residual.png" HEIGHT=300 WIDTH=400></A></center>:))

1-4 ���ܤϤ���«�Ǥ���. ���ޤǤȰۤʤ�Τ� 3 ����(require 'numru/gphys/ggraph_on_merdional_section')
���ɲä���Ƥ��뤳�ȤǤ�. GPhys �ˤ� GPhys ���֥����������Ѥ�����⥸�塼�� GGraph ���Ѱդ���Ƥ��ޤ�.
GGraph �����Ѥ���ˤ��̾� 'numru/ggraph' �� require ����櫓�Ǥ���, ���Υץ������ǤϻҸ��̾�Υե�å���
�٥��ȥ�������Τ��ò������᥽�å�(GGraph::vector_on_merdional_section)�����Ѥ��� EP Flux ��ɽ�����ޤ�.
���Υ᥽�åɤ��ɲ��������Ƥ���ե����뤬 'numru/gphys/ggraph_on_merdional_section' �Ǥ�.

6-37 ���ܤǤ�, demo_NCEP_1.rb �⤷���� demo_NCEP_2.rb ��¹Ԥ����ݤ˽��Ϥ���� epflx_NCEP.nc ��
�����ȥǥ��쥯�ȥ��¸�ߤ��뤫�ݤ��Ǿ��ʬ�����Ƥ��ޤ�. ¸�ߤ�����Ϥ����餫��ɬ�פ��ѿ���
�����ץ󤷤ޤ�. �����Ǥ� EP Flux �� ������ʬ����ӱ�ľ��ʬ(epflx_y, epflx_z)�Ȥ���ȯ��(epflx_div),
�����ƻĺ���®(v_rmean, w_rmean) �򳫤��ޤ�. �⤷ epflx_NCEP.nc ��̵������ demo1 or 2 Ʊ�ͤ�
�ƥ��ȥǡ����򸵤˷׻����ޤ�. ���Υ��塼�ȥꥢ��˱�äƥǥ�ץ�������ư���������ϴ���
EP Flux etc. ��׻����Ƥ���Ȼפ��ޤ��Τ�, ̵�̤�ʤ�����ˤ��Τ褦�ʾ��ʬ���򤷤Ƥ���ޤ�.

40-45 ���ܤϳ�����������β������Ǥ�. 40 ���� DCL.gropn(1) �ϡ�DCL �Υ���ե��å����ֽ�����Ǥ�.
������ 1 �ξ��, ü�����̤�ɽ������ޤ�. �̾�ɬ�פʤΤϤ��ιԤΤߤǸ�ϥ桼���ι��ߤˤ��ޤ�. 
41, 42 ���ܤ������̽��Ϥ�Ԥ�, �����ΰ�νĲ����1:0.85 �ǻ��ꤷ�Ƥ��ޤ�. 43 ���ܤǥ������������
������ʸ���Ȥ��Ʋ�ᤷ�ʤ��褦�ˤ�, 44 ���ܤǥե���Ȥ˥ץ��ݡ�����ʥ�ե���Ȥ��Ѥ�,
45 ���ܤǺ�ɸ���ˤĤ���ʸ�����礭���� 0.6 �ܤˤ��Ƥ��ޤ�.

49-51 ���ܤǤ��ѿ�����ּ��˱�ä�ʿ�Ѥ��Ƥ��ޤ�. EP_Flux::ep_full_sphere  ����� EP_Flux::div_sphere
�����Ϥ����ѿ��ϻ��������˸��Υǡ�����Ʊ���ʻ������Υ���åɤ���äƤ���Τ�, �����Ǥ� 2001 ǯ 4 ��ʿ���ͤ�
���Ф��뤳�Ȥˤʤ�ޤ�.

53-66 ���ܤǤ��褤�賨�������ޤ�. 53 ���ܤ� U ��ɸ�� V ��ɸ���б�������, 54 ���ܤ� y ����
������������ǥץ��åȤ��뤳�Ȥˤ��ޤ�. 55, 56 ���ܤ� EP Flux ��ȯ���Υȡ��󤪤�ӥ��󥿡������褷�ޤ�.
�ʤ��Ť��������뤿��� GPhys::contour �� 2 ���ܤΰ����Ǥϲ��ڡ������ʤ��褦 false �����ꤵ��Ƥ��ޤ�.
57-59 ����(GGraph::vector_on_merdional_section)�� EP Flux �Υ٥��ȥ�������ޤ�. Ʊ�ͤ� 61-63 ���ܤ�
�ĺ���®����Ӽ���ή���ؿ���ʿ���ͤ��� 64-67 ���ܤǥ��󥿡�����ӥ٥��ȥ�������ޤ�.
��������ɽ���նᤪ��������Φ�����������ΰ�ϥ��顼���礭������տ�Ū�˥��åȤ��Ƥ���ޤ�(62-63 ����).

GGraph::vector_on_merdional_section �ˤĤ��ƾܤ������⤷�ޤ�. ���Υ᥽�åɤϥ٥�
�ȥ�� y ��ʬ, z ��ʬ���ֵ�Υ�פ����㤹����ˡ�ή��פ�ɽ������褦�ˤʤäƤ���
��. �㤨�лҸ��̾��®�٤� log-P ��ɸ�Ǥ�

    [ v, w ] = [ D(a\phi)/Dt, D(-H\ln\p/\p_00)/Dt ]

��ɽ���ޤ�. ���ΤȤ� [v,w] �θ�����������ɽ���ˤ�ξ�Ԥ��椬���פʤΤ�
U ��ɸ�ˤ����� ���� y �� a\phi, z �� -H\ln\p/\p_00 ��������Ƥ���ή���
ɽ�����뤳�Ȥˤʤ�ޤ�. EP Flux �ξ���

   [Fy, FZ] = \sigma\cos\phi [ ... - v'u' , ... - w'u' ]

��, ξ�Ԥζ�����ʬ������� [v, w] �����㤷�Ƥ��ޤ��Τ�, ®�٤�Ʊ�ͤ�
�������򤷤Ƥ���ή�줬ɽ���Ǥ���櫓�Ǥ�. GGraph::vector_on_merdional_section
�ǤϺ�ɸ���ϸ���Ȥ� GPhys ���֥������Ȥ����ļ� (Grid) �򻲾Ȥ��ƽ񤭤ޤ���,
�٥��ȥ����ʤǤ� y ���� a\phi, z ���� z �Ȥ��ƺ�������ޤ�. 

((:<b>:))
((:<font color=rgb(87,153,51)>:))

�ޤȤ� - ��ñ��������

(1) �Ҹ��̾�ǥ٥��ȥ������ʤ� GGraph::vector_on_merdional_section 
    * require "numru/ggraph_on_merdional_section" ��˺�줺��
  
((:</font>:))
((:</b>:))

==������Ŭ��ǽ��

����ޤǤ� GPhys::EP_Flux �δ���Ū�ʻȤ����β���Ͻ���Ǥ�. ���������
�⥸�塼��ξܺ٤��Τꤿ���͸�����, GPhys::EP_Flux ��������Ŭ��ǽ�Ϥ�
�׻����٤��ä򤷤����Ȼפ��ޤ�. �ޤ���Ŭ��ǽ�ϤˤĤ��ƤǤ�.

===�������Ф��������

((<�ޤ��ϻȤäƤߤ褦>))�� GPhys::EP_Flux::ep_full_sphere ��������®, ������®,
��ľ��(omega), ���٤�����ˤȤ뤳�Ȥ��������ޤ���. �ޤ�, ���٤�����˲��̤�
Ϳ���뤳�Ȥ��Ǥ��뤳��, ���̤�Ϳ�������� 5 ���ܤΰ����� false �Ȥ��뤳�Ȥ�
�������ޤ���. ������Τˤ�, 5 ���ܤΰ����� true �ʤ�Х⥸�塼���������
4 ���ܤΰ����� GPhys ���֥������Ȥ�
((<�ɥ������|URL:document/document.html>)) ��(1.6)�����Ѥ��Ʋ��̤��Ѵ�����,
EP Flux �η׻��򤷤Ƥ��ޤ�.

�ޤ� omega ������˱�ľ��® w ��ľ�ܰ�����Ϳ���뤳�Ȥ��Ǥ��ޤ�.
�������ʤ��鲹�٤Τ褦�˰�����Ϳ����ե饰��ɬ��ͭ��ޤ���.
�¤� GPhys::EP_Flux ��Ϳ����줿GPhys ���֥������Ȥ�ñ�̤򸵤�, ���Υ�
�֥������Ȥ�ʪ���̤����Ԥ���ưŪ��Ƚ�̤��ޤ�!! (������ GPhys::EP_Flux ��������
������Ǥ�). �ʲ��� GPhys::EP_Flux::ep_full_sphere �ǸƤӽФ�, omega �� w ����
������᥽�åɤǤ�.

     569       def to_w_if_omega(gp, z) # it is only for z coordinate!!!
     570         gp_units = gp.data.units
     571         if gp_units =~ Units.new("Pa/s")
     572           pr = @@p00*exp(-z/@@scale_height)
     573           gp_un = gp_units
     574           pr = pr.convert_units(gp_un*Units.new('s'))
     575           gp = gp*(-@@scale_height/pr)
     576           gp.data.rename!("wwnd")
     577           gp.data.set_att('long_name', "log-P vertical wind")
     578         elsif gp_units =~ Units.new("m/s")
     579           gp = gp.convert_units(Units.new('m/s'))
     580         else
     581           raise ArgumentError,"units of gp.data (#{gp.data.units})
     582                                must be dimention of pressure/time
     583                                                  or length/time."
     584         end
     585         return gp
     586       end

570 ���ܤǱ�ľ���Ȥ���Ϳ����줿 GPhys ���֥������Ȥ�ñ�̤������, 571-584 ��Ƚ
��, �ơ��н褷�Ƥ��ޤ�. ñ�̤�"Pa/s"�������������Ǥ����
((<�ɥ������|URL:document/document.html>)) �� (1.5) �����Ѥ��� w ���Ѵ���,
"m/s"�������������Ǥ���� "m/s" �˴������Ʒ׻����Ѥ��ޤ�. �����ɤ���μ������
���ʤ��ʤ��, ���顼��λ������ͤˤʤäƤ��ޤ�. �ܥ⥸�塼��ǤϤ��Τ褦��ñ��
����Ȥ��Ƴ�ʪ���̤�Ƚ�̤�Ԥ��ޤ�. ���� or ���̤������Ȥ� "K" ��������������
���ĤΤǶ��̤Ǥ���, ���ʤ��ե饰����Ƚ�Ǥ��뤳�Ȥˤ��Ƥ��ޤ���, ����ʳ�����
�߹���� GPhys ���֥������Ȥ����ԤʤΤ�, �桼�����Τ�ʤ��Ȥ� GPhys::EP_Flux ��
��������������Ƥ���ޤ�. 

===�ǡ����μ��μ�ưȽ��

�ǡ����κ�ɸ���ˤĤ��Ƥ� GPhys::EP_Flux �Ͻ�����б�����ޤ�.
�ʲ��� GPhys::EP_Flux::ep_full_sphere �����ǸƤӽФ����, p �� log-P ���Ѵ�����
�᥽�åɤǤ�. omega => w �ξ���Ʊ�ͤ�, ñ�̤���Ƚ�̤��ޤ�.

     599       def to_z_if_pressure(gp_z)
     600                             # number in units is not considerd operater as log.
     601         if ( gp_z.data.units =~ Units.new('Pa') )
     602           p00 = @@p00.convert(gp_z.units)
     603           gp_z = -@@scale_height*log(gp_z/p00)
     604           gp_z.data.set_att('long_name', "z").rename!("z")
     605         elsif ( gp_z.data.units =~ Units.new('m') )
     606           gp_z = gp_z.convert_units(Units.new("m"))
     607         else
     608           raise ArgumentError,"units of gp_z (#{gp_z.data.units})
     609                                must be dimention of pressure or length."
     610         end
     611         return gp_z
     612       end

��ʿ���Ϥ��줾�����, ���٤ȳ��٤�ñ�̤Ȥ��ƻ��ꤵ��ͤФʤ�ޤ���.
���٤�ñ�̤� radian, degree ���������б����Ƥ��ޤ�. �ʲ���
GPhys::EP_Flux::ep_full_sphere �����ǸƤӽФ���� degree �� radian ���Ѵ�����᥽�åɤǤ�.
 
     629       def to_rad_if_deg(gp)
     630         if gp.data.units =~ Units.new("degrees")
     631           gp = gp.convert_units(Units.new('rad'))
     632           gp.units = Units[""]
     633           gp
     634         elsif gp.data.units =~ Units.new('rad')
     635           gp.data = gp.data.copy
     636           gp.data.units = Units[""]
     637           gp
     638         else
     639           raise ArgumentError,"units of gp #{gp.data.units} must be equal to deg or radian."
     640         end
     641         return gp
     642       end

��������Ѵ��᥽�åɤ�, ���ݡ����оݳ���ñ�̤��Τ������ϥ��顼��λ���ޤ�.
�դˤ����Х��顼���Ǥ��ʤ��¤�, �桼����ʪ���̤�ñ�������Τ�ʤ��Ȥ�, �⥸�塼���
��Ǥ���Ƿ׻�����ޤ�.


===�ѥ�᡼�����ѹ�

GPhys::EP_Flux �Ǥϰʲ��˼����⥸�塼���ѿ����������Ƥ��ޤ�.

     cp             = 1004                       # �絤��������Ǯ [J.K-1.kg-1]. 
     gas_const      = 287.0                      # �����絤�ˤ�����ñ�̼��̤�����ε������ [J.K-1.kg-1]
     
     @@scale_height = UNumeric.new(7000,  "m")   # ��������ϥ���(log ����)
     @@radius       = UNumeric.new(6.37E6,"m")   # ����Ⱦ��
     @@rot_period   = UNumeric.new(8.64E4,"s")   # ��ž����
     @@p00          = UNumeric.new(1.0E5,"Pa")   # ���Ȱ���(��ɽ���絤��������)
     @@g_forces     = UNumeric.new(9.81, "m.s-2")# ���ϲ�®��

�ǥե�����ͤ������ϵ���ͤǤ�. �嵭�Υѥ�᡼�������ƥ��������᥽�åɤ�
�������Ƥ����ѹ�����ǽ�Ǥ�(�ܺ٤� ((<�ɥ������|URL:ep_flux.html>)) �򻲾Ȳ�����. )
����ˤ���ϵ�ʳ��������ˤ����� EP Flux ��׻����뤳�Ȥ��ǽ�ˤʤ�ޤ�. 

((:<b>:))
((:<font color=rgb(87,153,51)>:))

�ޤȤ� - Ŭ��ǽ��

* ���� �� omega �����Ǥʤ������� �� ��ľ®�� (w = -H\omega/p) ������˼���
* ��ľ��ɸ�� p �����Ǥʤ� z = -H logp ���Ȥ���ʤ����⼫ưȽ�̡�
* ��ž����������Ⱦ�¤⥹������ϥ��Ȥ��Ѥ����롣
  ==> �ϵ�ʳ��ˤ�Ŭ�Ѳ� ��â������˸¤��
  
((:</font>:))
((:</b>:))

((:<hr>:))


##############################################################################
         
=end