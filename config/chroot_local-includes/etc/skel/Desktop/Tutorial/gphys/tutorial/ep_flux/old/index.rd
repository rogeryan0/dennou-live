=begin
= GPhys::EP_Flux ���塼�ȥꥢ��

�͸�����, ��Ƿ����

����: 2004 ǯ 9 ��

�ǽ�����: 2004 ǯ 10 �� 26 ��

== �ܼ�
((:<ol class="contents">:))
<<< index.hindex.rd
((:</ol>:))

==�Ϥ����

GPhys::EP_Flux ����®����Ӳ��٤� GPhys ���֥������Ȥ���Ҹ��̾����������
Elliassen-Palm Flux (EP Flux) �䤽�� divergence ��׻�����ؿ��򽸤᤿�⥸��
����Ǥ�. ����Ū�ˤ�, 3 ������ Plumb Flux �� Takaya-Nakamura Flux��׻�����
�ؿ����ɲä����ͽ��Ǥ�. �ܥ��塼�ȥꥢ��Ǥ�, GPhys::EP_Flux �λȤ���(����
��)�ȼ�ʬ����������ؿ��� GPhys::EP_Flux ���ɲä�����ˡ��GPhys �ѥ⥸�塼��
�γ�ȯ�˴ؤ��� TIPS(��ȯ��) ��Ҳ𤷤ޤ�.


==������


�����Ǥ� GPhys::EP_Flux ��������ˡ�䤽������, ������Ѥ��������β����Ԥ���
��.

===����
GPhys::EP_Flux �� GPhys �� cvs ��(2004/08/26 ������) �˰�¸���Ƥ��ޤ�.
���Ѥ�������Ϥޤ�������򥤥󥹥ȡ��뤷�Ƥ�������.

GPhys::EP_Flux �����Ѥ���ˤϰʲ��Υѥå�������ʲ��λ񸻤�ɬ�פǤ�.

* ((<numru-derivative|URL:http://www.gfd-dennou.org/arch/ruby/products/numru-derivative/numru-derivative.0.1.2.tar.gz>))
* ((<ep_flux|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/ep_flux.0.0.2.3.tar.gz>))
  
�ʤ� GPhys::EP_Flux �� ���� GPhys �ΥС������� ���Τ˼����ޤ�ޤ�.
(2004 ǯ 9 �� 7 �����ߤ� GPhys �ǿ��Ǥ� ver.0.3.3)
���ޤ����˻Ȥ������Ȥ�������, Ÿ�������ǥ��쥯�ȥ���� install.rb ��¹Ԥ��Ƥ�������.

===�ܥ��塼�ȥꥢ����Ѥ���ǡ���

�ܥ��塼�ȥꥢ��Ǥϥƥ��ȥǡ����Ȥ���, NCEP �Ʋ��Ϥˤ�� 2001 ǯ 4 �������
����®����Ӳ��٤Υǡ�����Ȥ��ޤ�.
((<������|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/tutorial/testdata.tar.gz>))
���������κ���ѥǥ��쥯�ȥ�˥�������ɤ���Ÿ�����Ƥ�������.
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

===����Ū�ʻȤ���

�ޤ��ϰʲ��Υץ�����¹Ԥ���, EP_Flux �Ȥ��� divergence ��׻����Ƥߤޤ�
�礦. ���Υץ����ϥƥ��ȥǡ������ɤ߹���ǻҸ��̾�� EP_Flux ��׻���,
����ˤ��� divergence �����, netCDF �ե�����(epflx_NCEP.nc)�Ȥ�����¸����
��ΤǤ�. �ʹ��Ǥ꤬�ʤ��¤�, �ܥ��塼�ȥꥢ����Ѥ���ץ��������ƥѥå�
��������°����ǥ�ץ�������Ѥ��ޤ�. �ʤ����ֹ���������ؤΤ���ˤĤ���
��Ƥ���, �ºݤϤ���ޤ���.

((<demo_NCEP_1.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/tutorial/demo/demo_NCEP_1.rb>)):
      1  require 'numru/gphys'
      2  require 'numru/gphys/ep_flux'
      3  include NumRu
      4
      5  datadir = "./testdata/"
      6  gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
      7  gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
      8  gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
      9  gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
      10
      11 epflx_y, epflx_z, v_rmean, w_rmean, gp_lat, = ary =
      12           GPhys::EP_Flux::ep_full_sphere(gp_u, gp_v, gp_omega, gp_t, true)
      13 gp_lat.rename('phi')
      14
      15 epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
      16
      17 ary.each{|gp|                                  #  This part will not
      18   gp.data.att_names.each{|nm|                  #  be needed in future.
      19     gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
      20   }                                            #  needed if the valid
      21 }                                              #  range is wide enough)
      22
      23 ofile = NetCDF.create("epflx_NCEP.nc")
      24
      25 ary.each{|gp|
      26   GPhys::IO.write(ofile, gp)
      27 }
      28 GPhys::IO.write(ofile, epflx_div)
      29
      30 ofile.close

1-3 ���ܤϤ�����֤���ˡ����ʬ�Ǥ�. 1-2 ���ܤ�GPhys::EP_Flux �����Ѥ��뤿��
��ɬ�פʥ饤�֥����ɤ߹���Ǥ��ޤ�. 'numru/gphys' �� 'numru/gphys/ep_flux'
�������Ǥ�ƤФ��Τ�, 2 ���ܤ����Ǥ� OK �Ǥ�. 3 ���ܤȤ��뤳�Ȥ� NumRu �ʲ�
��̾�����֤�Ǽ����Ƥ���⥸�塼���, NumRu:: ���ά���ƸƤ֤��Ȥ��Ǥ��ޤ�. 

6-9 ���ܤǥƥ��ȥǡ����� GPhys ���֥������ȤȤ��Ƴ����Ƥ��ޤ�. ������,
�ºݤ˥�����ɤ߹��ޤ��Τϱ黻��Ԥ���ʬ(13-14 ����)��, ���λ����Ǥϻ��ꤵ
�줿�ե���������ѿ���ؤ��Ƥ�������Ǥ�. �����Ǥ� gp_u, gp_v, gp_omega, gp_t
���줾���������®, ������®, ��ľ��®, ������������Ƥ��Ƥ��ޤ�.


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
gp_lat �Ȥ����ѿ����������Ƥ��ޤ�. ���դ� , �ǽ���äƤ��뤳�Ȥ���դ��Ƥ�������.
����ˤ�� ep_full_sphere �� 6 ���ܰʹߤ�����ͤ��ΤƤ��ޤ�. �⤷�� "," ��ȴ��
���� gp_lat ������Ȥ��� 5 - 11 ���ܤΥ��֥������Ȥ���Ǽ����뤳�Ȥˤʤ�ޤ�.



13 ���ܤǤ� gp_lat �Υǡ�����ʬ��̾���� "phi" ���֤������Ƥ��ޤ�(�ǥե���Ȥ� "lat").
��ҤΤȤ���, GPhys ���֥������Ȥ��ѿ���̾����°���Ȥ��ƻ��Ĥ��Ȥ��Ǥ���ΤǤ���,
�ѹ����������Ϥ��Τ褦�ˤ���ФǤ��ޤ�.

15 ���ܤ� GPhys::EP_Flux::div_sphere ��Ƥ�� EP Flux ��ȯ����׻����Ƥ��ޤ�.
div_sphere �ϻҸ��̾�� y-z ʿ�̤ˤ�����ȯ����׻�����᥽�åɤ�, ������ EP_Flux
�ʳ���ʪ���̤ˤ����ѤǤ��ޤ�.

17-21 ���ܤϥ��ƥ졼�����Ѥ�������ͤ����ƤΥǡ�����ʬ��°�� "valid_hoge" ��
������Ƥ��ޤ�. "valid_hoge" ��ͭ���ϰϤ˴ؤ���°����, ���ꥸ�ʥ�Υǡ�����
���ä�°���Ǥ�. GPhys ��°���ͤ��ۤ˻��ꤷ�ʤ��¤�, �ǽ���ɤߤ�����ǡ�����
�Ф��Ƥ���Τ�, �黻��������Ѳ�������(�����ͭ���ϰϤ�����ˤ�����ޤ���)
�����Թ礬�����Ƥ��ޤ��ޤ�. ���˹����ϰϤ�Ϳ�����Ƥ��ʤ��¤�, �����ͤ�
�ѹ�����ʤ��ƤϤʤ�ޤ���. ����Ū�� GPhys ���Ȥ�����������н褷�ޤ�.

23-30 ���ܤ�, �׻���̤� NetCDF �ե�����Ȥ�����¸���ޤ�. 23 ���ܤ���¸�ե�����
�������, 25-28 ���ܤǸġ��� GPhys ���֥������Ȥ�񤭹��ߤޤ�. GPhys::IO.write
�� GPhys ���֥������Ȥ� NetCDF �˽񤭹������ѤΥ᥽�åɤǤ�. �Ǹ�� 30 ���ܤ�
��¸�ե�����򥯥������Ƥ��ޤ�. ���ιԤ�̵�����������¸��λ���뤳�Ȥ��Ǥ�
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

�ޤȤ� - ����Ū�ʻȤ��� 

(1) EP Flux ��׻�����ˤ� require "numru/gphys/ep_flux" ����
    GPhys::EP_Flux::ep_full_sphere ��Ƥ�
(2) ɬ�פ� GPhys ���֥������Ȥ�������®, ������®, ��ľ��®, ����(����)
(3) 5 ���ܤΰ����ϲ��٤ʤ� true, ���̤ʤ� false. �ǥե���Ȥ� true.
(4) ���ϥ��֥������Ȥμ�������Ʊ��. �ǥե���Ȥ�["����", "����", "��ľ��"]�ν�.
(5) ep_full_sphere ������ͤν��֤����
    * ���
      epflx_y, epflx_z, v_rmean, w_rmean, phi, z, uwnd, theta, uv_dash,
      vt_dash, uw_dash, dtheta_dz

((:</font>:))
((:</b>:))
      
===����ΨŪ�ʻȤ���

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
�ǤϤɤ�����и�ΨŪ�ʤΤǤ��礦��? EP_Flux ��, �Ȥ�����ꤵ�줿����ˤ�
����ǡ������Ф��ƽ����򤷤ޤ�. ����Υƥ��ȥǡ����ϻ��������� 120 ��
�γʻ�������äƤ��ޤ��Τ�, ���ּ��˱�äƥ롼�פ�󤹤��ɤ��Ǥ��礦.
��������з׻���ɬ�פʥǡ�������Ŭ���������ɤ߽Ф�, �׻������꼡��
�ե�����˽񤭽Ф�. �����Ƽ��Υǡ������ɤ߹���...�Ȥ����褦�ˤ��Ƥ�����
���������ˤʤ�ޤ�. ����ǤϾ嵭�θ�Ψ���� GPhys::IO �Υ��ƥ졼����
�Ѥ��Ƽ���������ˡ��Ҳ𤷤ޤ�. �ʲ��Υץ���������������.

((<demo_NCEP_2.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/tutorial/demo/demo_NCEP_2.rb>)):
      1  require 'numru/gphys'
      2  require 'numru/gphys/ep_flux'
      3  include NumRu
      4
      5  datadir = "./testdata/"
      6  gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
      7  gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
      8  gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
      9  gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
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
      23   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
      24
      25   ary.each{|gp|                                  #  This part will not
      26     gp.data.att_names.each{|nm|                  #  be needed in future.
      27       gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
      28     }                                            #  needed if the valid
      29   }                                              #  range is wide enough)
      30
      31   if i==1    # time independent => write only once
      32     gp_lat.rename('phi')
      33     GPhys::IO.write(ofile, gp_lat)
      34     GPhys::IO.write(ofile, gp_z)
      35   end
      36   [ epflx_y, v_rmean, w_rmean, epflx_z, epflx_div, u_mean, theta_mean,
      37     uv_dash, vt_dash, uw_dash, dtheta_dz ]
      38 }
      39
      40 ofile.close

���Υץ���������Υǥ�ץ���� (demo_NCEP_1.rb) Ʊ�ͤ�, �ƥ��ȥǡ�������
�Ҹ��̾�� EP Flux ¾��׻���, nc �ե��������¸�����ΤǤ�. ������, 13 ���ܰʹ�
���礭���ۤʤ�ޤ�. ��˽Ҥ٤��׻��θ�Ψ����ޤ뤿��, GPhys �Υ��ƥ졼��������
���Ƥ��ޤ�. 

1-11 ���ܤ� demo1 ��Ʊ�����饤�֥�ꤪ��ӥǡ������ɤ߹��ߤ���¸�ե�����Υ����ץ��
�ԤäƤ��ޤ�.

13-14 ���ܤϸ�ҤΥ��ƥ졼����ʬ�ν����οʹ��ٹ礤����Ϥ��뤿����������Ǥ�.
13 ���� (nt = gp_u.shape[-1]) �� gp_u �ΰ��ֺǸ�μ����Υ���åɿ����֤��ޤ�. ���Υƥ���
�ǡ����Ǥϻ��� time �Υ���åɿ� 120 �� nt ���������ޤ�.

15-38 ���ܤ� demo1 �� 11-28 ���ܤ򥤥ƥ졼�����Ѥ��Ƽ������Ƥ��ޤ�.
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
      23   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
      ...
      36   [ epflx_y, v_rmean, w_rmean, epflx_z, epflx_div, u_mean, theta_mean,
      37     uv_dash, vt_dash, uw_dash, dtheta_dz ]
      38 }


15 ���ܤ���������. GPhys::IO.each_along_dims_write �������� (�����Ǥ� [gp_u, ..., gp_t]) �ˤ�
���ƥ졼����ǽ������� GPhys ���֥������Ȥ����Ǥ˻�������, �⤷���� GPhys
���֥������ȼ��Ȥ��Ϥ��ޤ�. �� 2 �����ˤ���¸�оݤΥե����륪�֥������Ȥ��Ϥ��ޤ�.
�� 3 �����ʹߤϥ롼�פ�󤹼�����̾���⤷���Ͽ����ǻ��ꤷ�ޤ�. �����Ǥϰ��ֺǸ��
�����Ǥ��� time �������Ƥޤ�. 

16 ���ܤǤ� gp_u, gp_v, gp_omega, gp_t ����ּ��˱�äƥ��饤���������֥������Ȥ�
���줾�� u, v, omega, t �Ȥ��ƥ��ƥ졼������Ϥ��Ƥ��ޤ�. 

20-23 ���ܤ� demo1 Ʊ�� EP_Flux::ep_full_sphere ��ƤӽФ��Ƴ��ѿ��η׻���ԤäƤ��ޤ�.
ͣ��ۤʤ�Τ�, Ϳ�������������ּ��˱�äƥ��饤�����줿���֥������ȤǤ������Ǥ�.

36-37 ���ܤ�, ��¸�оݥե����� ofile ����¸�����ѿ������Ǥ˴ޤ��������������
���ƥ졼���Υ֥�å����Ĥ��ޤ�. ���֤˰�¸�����ѿ�, ���ʤ�����ּ�������ѿ�
�Τߵ��ܤ��Ƥ��뤳�Ȥ���դ��Ƥ�������. (���֤˰�¸���ʤ��ѿ�, �����Ǥ� gp_lat ��, ��
�롼����˾�񤭤����̵�̤ʤΤ�, 31 - 35 ���ܤΤ褦�� 1 ������ե�����˽񤭹���褦
���פ��Ƥ��ޤ�.)

40 ���ܤǥե�����Υ�������ԤäƤ��ޤ�. 

����Ǥϥץ�����¹Ԥ��Ƥߤޤ��礦. �¹����ɸ����Ϥˤϰʲ��Τ褦�ˤʤ�Ȼפ��ޤ�.
18 ���ܤˤƿʹԾ�����롼�פβ�����Ѥ���ɽ�����Ƥ��뤿��Ǥ�. ������ʬ�Ϸ褷��ɬ�ܤǤ�
����ޤ���, �׻��˻��֤Τ�����ץ����ˤ����Ƥϼ����������Ǥ���.

  % ruby demo_NCEP_2.rb

    processing 7 / 120 ..
    processing 14 / 120 ..
    ...
    processing 120 / 120 ..

����˽�λ������ ncdump ���Ѥ��ƥإå��������Ƥߤޤ��礦. demo_NCEP_1.rb �����Ϥ��� nc
�ե������Ʊ���Ǥ��뤳�Ȥ��狼��ޤ� (�ѿ��ν��֤ϰ㤦���⤷��ޤ���, �����Ϥ�����������).

�ʾ�ǥ��ƥ졼�����Ѥ����ץ�����Ҳ𤷤ޤ���. ���ƥ졼����Ȥ���, ����������¾,
�ġ����ѿ��ϻ��ּ��˱�äƥ��饤��������Τ�����˽������Ƥ������Ȥˤʤ�Τ�, ���ϸ�Ψ
�β������ޤ�ޤ�. ΢���֤���,
����ʥǡ����򰷤��Ȥ���, ���äڤ�ˤ��ȥ��꡼���������Ʊ���˽��ϸ�Ψ�Ⲽ�����ǽ��
������Τ�, ���ƥ졼������Ȥ��Ȥ������ȤǤ�. �Ȥ����櫓������ΤޤȤ�Ǥ�. 

((:<b>:))
((:<font color=rgb(87,153,51)>:))

�ޤȤ� - ����ΨŪ�ʻȤ��� 

(1) ����ǡ����ϥ��ƥ졼����Ȥäƽ���
    * �Ȥ�����GPhys::NetCDF_IO.each_along_dims_write(gphyses, ofile, dims)
(2) GPhys::EP_Flux ���Ѥ�����, ���ּ�����ĥǡ�����ͭ��
  
((:</font>:))
((:</b>:))

    
===��ñ��������

����ǤϤ��褤�� GPhys::EP_Flux �ǽ��Ϥ����ʪ���̤򳨤ˤ��Ƥߤޤ��礦. EP Flux ��
�������ˤϥ٥��ȥ�ޤ��٥��ȤǤ�. �����Ǥ� EP Flux ��٥��ȥ�, �����С������󥹤�
���󥿡�����ӥȡ���ǽŤͤ�������ץ�����Ҳ𤷤ޤ�. ���ޤǤΥץ�������Ĺ
���Ǥ���, 01-37 ���ܤϤ���ˡ����ӥǡ������ɤ߹�����ʬ��, ���פʤΤϸ�Ⱦ��ʬ�Ǥ��Τ�
���¿�������.

((<demo_NCEP_3.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/tutorial/demo/demo_NCEP_3.rb>)):
      1  require 'numru/gphys'
      2  require 'numru/gphys/ep_flux'
      3  require 'numru/gphys/ggraph_on_merdional_section'
      4  include NumRu
      5
      6  epflx_fnm = './epflx_NCEP.nc'
      7 
      8  if File::exist?(epflx_fnm)
      9 
      10   epflx_y =   GPhys::IO.open(epflx_fnm,  'epflx_y')
      11   epflx_z =   GPhys::IO.open(epflx_fnm,  'epflx_z')
      12   epflx_div = GPhys::IO.open(epflx_fnm,  'epflx_div')
      13   v_rmean =   GPhys::IO.open(epflx_fnm,  'v_rmean')
      14   w_rmean =   GPhys::IO.open(epflx_fnm,  'w_rmean')
      15
      16 else
      17
      18   datadir = "./testdata/"
      19   gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
      20   gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
      21   gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
      22   gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
      23
      24   epflx_y, epflx_z, v_rmean, w_rmean= ary =
      25     GPhys::EP_Flux::ep_full_sphere(gp_u, gp_v, gp_omega, gp_t, true)
      26
      27   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
      28
      29   ary.each{|gp|                                  #  This part will not
      30     gp.data.att_names.each{|nm|                  #  be needed in future.
      31       gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
      32     }                                            #  needed if the valid
      33   }                                              #  range is wide enough)
      34
      35 end
      36
      37
      38 DCL.gropn(1)
      39 DCL::sglset('LFULL', true)                       # use full area
      40 DCL::slrat(1.0, 0.85)                            # set aspect ratio of drwable area
      41 DCL.sgpset('lcntl', false)                       # don't rede control character.
      42 DCL.sgpset('lfprop',true)                        # use prportional font
      43 DCL.uzfact(0.6)                                  # set character size
      44
      45 ## show vector (Fy, Fz)
      46 # monthly mean
      47 fy = epflx_y.mean('time')
      48 fz = epflx_z.mean('time')
      49 epdiv = epflx_div.mean('time')
      50
      51 GGraph::set_fig('view'=>[0.15, 0.85, 0.25, 0.55])
      52 GGraph::set_fig('itr'=>2)                        # contour && tone only
      53 GGraph::contour(epdiv)                           # contour
      54 GGraph::tone(epdiv, false)                       # tone
      55 GGraph::vector_on_merdional_section(fy, fz, false,
      56    'fact'=>3.0,'xintv'=>1,'unit'=>true, 'annot'=>false
      57                                     )
      58
      59 ## show residual mean merdional circulation (cut nearly surface and Antarctica)
      60 vrm = v_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
      61 wrm = w_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
      62 GGraph::vector_on_merdional_section(vrm, wrm, true,
      63                         'fact'=>5.0,'xintv'=>1,'unit'=>true, 'annot'=>false
      64                                     )
      65 DCL.grcls

�¹Ԥ���Ȥޤ��ʲ��γ������̤����褵��ޤ�.

((:<center><A HREF="./epflx.png"><IMG SRC="epflx.png" HEIGHT=300 WIDTH=400></A></center>:))

���ڡ����������Ǹ����뤫���򥯥�å�����ȼ��γ������Ϥ��ޤ�.

((:<center><A HREF="./residual.png"><IMG SRC="residual.png" HEIGHT=300 WIDTH=400></A></center>:))

1-4 ���ܤϤ���«�Ǥ���. ���ޤǤȰۤʤ�Τ� 3 ����(require 'numru/gphys/ggraph_on_merdional_section')
���ɲä���Ƥ��뤳�ȤǤ�. GPhys �ˤ� GPhys ���֥����������Ѥ�����⥸�塼�� GGraph ���Ѱդ���Ƥ��ޤ�.
GGraph �����Ѥ���ˤ��̾� 'numru/ggraph' �� require ����櫓�Ǥ���, ���Υץ����ǤϻҸ��̾�Υե�å���
�٥��ȥ�������Τ��ò������᥽�å�(GGraph::vector_on_merdional_sphere)�����Ѥ��� EP Flux ��ɽ�����ޤ�.
���Υ᥽�åɤ��ɲ��������Ƥ���ե����뤬 'numru/gphys/ggraph_on_merdional_section' �Ǥ�.

6-35 ���ܤǤ�, demo_NCEP_1.rb �⤷���� demo_NCEP_2.rb ��¹Ԥ����ݤ˽��Ϥ���� epflx_NCEP.nc ��
�����ȥǥ��쥯�ȥ��¸�ߤ��뤫�ݤ��Ǿ��ʬ�����Ƥ��ޤ�. ¸�ߤ�����Ϥ����餫��ɬ�פ��ѿ���
�����ץ󤷤ޤ�. �����Ǥ� EP Flux �� ������ʬ����ӱ�ľ��ʬ(epflx_y, epflx_z)�Ȥ���ȯ��(epflx_div),
�����ƻĺ���®(v_rmean, w_rmean) �򳫤��ޤ�. �⤷ epflx_NCEP.nc ��̵������ demo1 or 2 Ʊ�ͤ�
�ƥ��ȥǡ����򸵤˷׻����ޤ�. ���Υ��塼�ȥꥢ��˱�äƥǥ�ץ�����ư���������ϴ���
EP Flux etc. ��׻����Ƥ���Ȼפ��ޤ��Τ�, ̵�̤�ʤ�����ˤ��Τ褦�ʾ��ʬ���򤷤Ƥ���ޤ�.

38-43 ���ܤϳ�����������β������Ǥ�. 38 ���� DCL.gropn(1) �ϡ�DCL �Υ���ե��å����ֽ�����Ǥ�.
������ 1 �ξ��, ü�����̤�ɽ������ޤ�. �̾�ɬ�פʤΤϤ��ιԤΤߤǸ�ϥ桼���ι��ߤˤ��ޤ�. 
39, 40 ���ܤ������̽��Ϥ�Ԥ�, �����ΰ�νĲ����1:0.85 �ǻ��ꤷ�Ƥ��ޤ�. 41 ���ܤǥ������������
������ʸ���Ȥ��Ʋ�ᤷ�ʤ��褦�ˤ�, 42 ���ܤǥե���Ȥ˥ץ�ݡ�����ʥ�ե���Ȥ��Ѥ�,
43 ���ܤǺ�ɸ���ˤĤ���ʸ�����礭���� 0.6 �ܤˤ��Ƥ��ޤ�.

47-49 ���ܤǤ��ѿ�����ּ��˱�ä�ʿ�Ѥ��Ƥ��ޤ�. EP_Flux::ep_full_sphere  ����� EP_Flux::div_sphere
�����Ϥ����ѿ��ϻ��������˸��Υǡ�����Ʊ���ʻ������Υ���åɤ���äƤ���Τ�, �����Ǥ� 2001 ǯ 4 ��ʿ���ͤ�
���Ф��뤳�Ȥˤʤ�ޤ�.

51-64 ���ܤǤ��褤�賨�������ޤ�. 51 ���ܤ� U ��ɸ�� V ��ɸ���б�������, 52 ���ܤ� y ����
����������ǥץ�åȤ��뤳�Ȥˤ��ޤ�. 53, 54 ���ܤ� EP Flux ��ȯ���Υȡ��󤪤�ӥ��󥿡������褷�ޤ�.
�ʤ��Ť��������뤿��� GPhys::contour �� 2 ���ܤΰ����Ǥϲ��ڡ������ʤ��褦 false �����ꤵ��Ƥ��ޤ�.
55-57 ����(GGraph::vector_on_merdional_sphere)�� EP Flux �Υ٥��ȥ�������ޤ�. Ʊ�ͤ� 60-61 ���ܤ�
�ĺ���®��ʿ���ͤ��� 62-64 ���ܤǥ٥��ȥ��ɽ�����Ƥ��ޤ�. ��������ɽ���նᤪ��������Φ
�����������ΰ�ϥ��顼���礭������տ�Ū�˥��åȤ��Ƥ���ޤ�(60-61 ����). 

GGraph::vector_on_merdional_sphere �ˤĤ��ƾܤ������⤷�ޤ�. ��˽Ҥ٤��Ȥ���, ���Υ᥽�åɤ�
�Ҹ��̾�Ρ�ή��פ�ɽ���٥��ȥ���Ф���Ŭ�ڤʥ�������󥰤�Ԥ����Ϥ����ΤǤ�. 

..now under construction..


===�׻�����

..now under construction..

===������ǥ�

..now under construction..

===F&Q

+ Q1. ���Ϥ���� GPhys ���֥������Ȥμ��Ϥɤ��ʤäƤ��ޤ���?

A1.

..now under construction..


1 �����ܤ˻���, 2 �����ܤ˰���, 3 �����ܤ˹��٤����äƤ��� GPhys ���֥������Ȥ����Ϥ���ˤϤɤ�����Ф��褤�Ǥ���?
(���μ����³��) ���!!! ���Ƥ� GPhys ���֥������ȤΥ���åɤ�Ʊ���Ǥʤ��Ȥ����ޤ���.
�㤨��, ������®�� 1000 mb ���� 10 mb ���������Ƥ��뤱��, ��ľ���� 100 mb����,
�׻����˥��顼��λ���ޤ�. �ޤ����륪�֥������Ȥ� 0 �����ܤ����٤������̤Υ��֥������Ȥϻ��֤�, �Ȥ����Τ⤤���ޤ���.
GPhys::EP_Flux �Ǥ�������®�Υ���ä������ƤΥ��֥������ȤΥ���åɤ��Ȼפ����ͤˤʤäƤ���Τ�,
�׻����ΤϤǤ��ޤ����ͤ����������ʤ�ޤ�. ���ϥ��֥������ȤˤϽ�ʬ��դ�ʧ�äƤ�������.

==��ȯ��

..now under construction.. 

=end
