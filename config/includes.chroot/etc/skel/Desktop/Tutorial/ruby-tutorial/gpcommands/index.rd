=GPhys���ޥ�ɥ��塼�ȥꥢ��
2006/03/09 ����������

GPhys�򥤥󥹥ȡ��뤹��Ȥˤϰʲ��Υ��ޥ�ɤ��Ȥ���褦�ˤʤ�ޤ���
GPhys�Υ��󥹥ȡ���ˤĤ��Ƥ�GPhys ���塼�ȥꥢ���((<���󥹥ȡ���|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/body-j.html#h2:install>))���������������

 * grads2nc_with_gphys: GrADS �����Υե������NerCDF�����Υե�������Ѵ����ޤ���
 * gdir_server: GDir�����С���Ω���夲�ޤ�
 * gdir_client: irb+GDir���饤�����
 * gpcat: GPhys�ե������Ϣ�� 
 * gpcut:  GPhys�Υ��饤���ʤ�
 * gplist: �ե�������ѿ��ȼ���ɽ��
 * gpmath: GPhys�ѿ��˿��شؿ���Ŭ��
 * gpmaxmin: GPhys�ѿ��κ��硦�Ǿ��ͤ�ɽ��
 * gpprint: GPhys�ѿ����ͤ�ɽ��
 * gpview: 1������2�����Υ���ե��å�����

���Υ��塼�ȥꥢ��Ǥ�gpview�ˤ��Ļ벽���濴�˲��⤷�Ƥ����ޤ���
((:<hr>:))
Contents
<<< TOC.rd
((:<hr>:))

==�ΤäƤ���������ʥ��ޥ�ɤ���
===�ե����������ѿ�̾�伴��Ĵ�٤� �� gplist ���ޥ�ɡ�
�Ļ벽�����˥���ץ�ǡ��� ((<T.jan.nc|URL:T.jan.nc>)) ����Ȥ��ɤΤ褦�ʤ�Τ�Ĵ�٤Ƥߤ褦�� netCDF �ե�����Ǥ���� ncdump -h ���뤳�ȤǴ�ñ��Ĵ�٤��ޤ���������� gplist ���Ѥ��Ƥߤޤ���

  % gplist T.jan.nc
����ȡ�
  T.jan.nc:
    lon   [lon=36]        'Longitude'     (degrees_east)
    lat   [lat=19]        'Latitude'      (degrees_north)
    level [level=9]       'Level' (millibar)
    T     [lon=36,lat=19,level=9] 'Temperature'   (degC)

��ɽ������ޤ��������(lon, lat, level)�Σ��Ĥμ������ĥǡ����Ȥ��� T �����뤳�Ȥ��狼��ޤ���

GrADS ����ȥ���ե�����䡢 grib �ե�����Ǥ�Ʊ�ͤˤ���Ĵ�٤뤳�Ȥ��Ǥ��ޤ���
����ץ�ǡ���((<T.jan.grib|URL:T.jan.grib>))�Ǥ��ᤷ�Ƥߤ褦��
  % gplist T.jan.grib
�����
  T.jan.grib:
    TMP   [lon=36,lat=19,level=9] 'Temperature'   (K)
��ɽ������ޤ���

===�ե����������ѿ����ͤ�ɽ������ �� gpprint ���ޥ�ɡ�
gpprint ���ޥ�ɤ�Ĥ����ȡ��ѿ����ͤ�ɽ�����Ǥ��ޤ���T.jan.nc �ե�����˽񤫤�Ƥ����ѿ� T ���ͤ�ɽ������ˤϲ��Τ褦�����Ϥ��ޤ���

  % gpprint T.jan.nc@T

===�ǡ����κǾ��������ͤ��Τꤿ����gpmaxmin ���ޥ�ɡ�
gpmaxmin ���ޥ�ɤ�Ȥ����Ȥ��ͤκǾ��������ͤ��Τ뤳�Ȥ��Ǥ��ޤ���

  % gpmaxmin T.jan.nc@T
�����
  T.jan.nc@T : max=33.2092704772949, at, 
       ...(total 1 points)...
  
  T.jan.nc@T : min=-77.9977569580078, at,  
       ...(total 1 points)...
�̰�40�٤ΰ���-���٥ǡ����κ��硦�Ǿ��ͤ��Τ�ˤϰʲ��Τ褦�ˤ��ޤ���
  % gpmaxmin T.jan.nc@T,lat=40
�����
  T.jan.nc@T,lat=40 : max=13.325665473938, at, 
       ...(total 1 points)...

  T.jan.nc@T,lat=40 : min=-62.9790420532227, at,  
       ...(total 1 points)...

==gpview�ˤ�뤪���2D�Ļ벽�ޤ�
===gpview�ˤ�뤪�ޤ����Ļ벽
����ץ�ǡ��� ((<T.jan.nc|URL:T.jan.nc>)) �ˤ��ѿ� T �����뤳�Ȥ��狼�ä��Τ���®gpview��ȤäƲĻ벽���Ƥߤ褦��
gpview �κǤ��ñ�ʻȤ������ϡ���gpview �ե�����̾@�ѿ�̾�٤Ǥ���

  % gpview T.jan.nc@T
�����Ϥ���Ȳ��Τ褦�ʿޤ�ɽ������뤳�ȤǤ��礦��

���Ϸ�̡�
((:<center><IMG SRC="T1000mb.png" width=640 height=480></center>:))

���Τ褦��3�����ǡ������Ф��ơ��ä��ڤ�����̤ʤɤ���ꤷ�ʤ��ä���硢�Ϥ��2�������Ф������褬�Ԥ��ޤ���
�ޤα����� level=1000millibar �Ȥ��뤳�Ȥ��顢 1000mb �̤β���ʬ�ۤ����褵�줿���Ȥ��狼��ޤ���
������ GrADS ����ȥ���ե����롢 grib �ե�����Ǥ�Ʊ�ͤˤ������褬�Ǥ��ޤ���

GrADS �ե�����ξ�硧
  % gpview T.jan.ctl@T

grib �ե�����ξ�硧
  % gpview T.jan.grib@TMP

===�ڤ�����̤���ꤷ������
�ѿ�̾��³����,(�����)�Ƕ��ڤä��ڤ�����̤������ϰϤ���ꤹ�뤳�Ȥ��Ǥ��ޤ���
�ޤ��Ϥ�����٤��ڤ�Ф��Ƥߤ褦��
  % gpview T.jan.nc@T,level=500

���Ϸ�̡�
((:<center><IMG SRC="T500mb.png" width=640 height=480></center>:))

�ޤα����� level=600millibar �Ȥ��뤳�Ȥ���դ��褦��
�����Ǥ� 500mb �Υǡ������ʤ��ä��Τǡ� 500mb �˶ᤤ 600mb �Υǡ��������褵��ޤ�����

===�����ϰϤλ��� �ء��ʥ����ˡ�
����������ϰϤ���ꤷ�Ƥߤޤ��礦���ϰϻ���ˤ� : (�����)��Ȥ��ޤ���
�㤨�� 0:10 �ϡ�0����10�ޤǤȤ�����̣�Ǥ���
���Υ��ޥ�ɤϡ����ä���Ʊ�����٤����0-180�١��̰�0-40�٤�����񤯤褦�˻��ꤷ�Ƥ��ޤ���
  % gpview T.jan.nc@T,level=500,lon=0:180,lat=0:40

���Ϸ�̡�
((:<center><IMG SRC="T500mb2.png" width=640 height=480></center>:))

===���˥᡼�����⤪��� ��anim ���ץ����reverse ���ץ�����
--anim ���ץ�������ꤹ�뤳�Ȥǥ��˥᡼�������ǽ�Ǥ������Υ��ޥ�ɤ�¹Ԥ��뤳�ȤǤ��줾��ι���(level)�����̤�Ϣ³���Ƹ��뤳�Ȥ��Ǥ��ޤ���
  % gpview --anim level T.jan.nc@T

����� --reverse ���ץ�������ꤹ�뤳�Ȥǵպ������˥᡼�������ǽ�Ǥ������Υ��ޥ�ɤ�¹Ԥ���� 10mb �̤������褵��ޤ���
  % gpview --anim level --reverse T.jan.nc@T

==��ä�2D����
===���󥿡������񤤤Ƥۤ��� ��noshade ���ץ�����
--noshade ���ץ������դ��뤳�Ȥˤ�äƿ��ɤ��ߤᤵ���뤳�Ȥ��Ǥ��ޤ���

  % gpview --noshade T.jan.nc@T
���Ϸ�̡�
((:<center><IMG SRC="T1000mb_noshade.png" width=640 height=480></center>:))

===���󥿡��ֳ֤λ��ꡡ��cint ���ץ�����
--cint���ץ����ˤ�äƥ��󥿡��ֳ֤���ꤹ�뤳�Ȥ��Ǥ��ޤ���
������Ǥϥ��󥿡��ֳ֤� 4K �����ꤷ�Ƥ��ޤ���
  % gpview --noshade --cint 4 T.jan.nc@T
���Ϸ�̡�
((:<center><IMG SRC="T1000mb_noshade_int4.png" width=640 height=480></center>:))

===���󥿡�������ͤ��ϰϤ���ꡡ��crange ���ץ�����
--crange ���ץ����ˤ�äƥ��󥿡�������ͤ��ϰϤ���ꤹ�뤳�Ȥ��Ǥ��ޤ���������Ǥ� 20�� ���� 40�� �ޤǤ������󥿡���������ޤ���

  % gpview --noshade --crange 20:40 T.jan.nc@T
���Ϸ�̡�
((:<center><IMG SRC="crange.png" width=640 height=480></center>:))

===���ɤ�������Ƥۤ�������nocont ���ץ�����
--nocont ���ץ������դ��뤳�Ȥˤ�ä���������񤯤Τ�ߤᤵ���뤳�Ȥ��Ǥ��ޤ�����

  % gpview --nocont T.jan.nc@T
���Ϸ�̡�
((:<center><IMG SRC="T1000mb_nocont.png" width=640 height=480></center>:))

===���ɤ�ֳ֤λ��� ��sint ���ץ�����
--sint ���ץ�������ꤷ�ޤ���
������Ǥϥ��󥿡��ֳ֤� 3K �����ꤷ�Ƥ��ޤ���
  % gpview --nocont --sint 3 T.jan.nc@T
���Ϸ�̡�
((:<center><IMG SRC="T1000mb_nocont_int3.png" width=640 height=480></center>:))

===�ɤ�櫓��Ԥ��ͤ��ϰϤ���ꡡ��srange ���ץ�����
--srange ���ץ����ˤ�ä��ɤ�ʬ���򤹤��ͤ��ϰϤ���ꤹ�뤳�Ȥ��Ǥ��ޤ���������Ǥ� 20�� ���� 40�� �ޤǤ����ɤ�櫓���Ԥ��ޤ���

  % gpview --nocont --srange 20:40 T.jan.nc@T
���Ϸ�̡�
((:<center><IMG SRC="srange.png" width=640 height=480></center>:))


===���󥿡����ͤ�ľ�ܻ��ꡡ��levels ���ץ�����
--levels ���ץ����ǥ��󥿡����ͤ�ľ�ܻ��ꤹ�뤳�Ȥ��Ǥ��ޤ���
������Ǥ�20�٤���30�٤ˤ�����2.5�٤����˥��󥿡���Ҥ��ޤ���
  % gpview --noshade  --levels -40,-30,-20,-10,0,10,20,22.5,25,27.5,30,40   T.jan.nc@T 
���Ϸ�̡�
((:<center><IMG SRC="level_sirokuro.png" width=640 height=480></center>:))

���Υ��ץ����Ͽ��ɤ�ˤ�Ŭ�Ѥ���ޤ���--noshade ���ץ����򳰤��Ƥߤޤ��礦��
  % gpview --noshade  --levels -40,-30,-20,-10,0,10,20,22.5,25,27.5,30,40   T.jan.nc@T 
���Ϸ�̡�
((:<center><IMG SRC="level_color.png" width=640 height=480></center>:))
���ɤ���������Ԥ��Ƥ��ޤ���

===�ƥ�٥�Υѥ�����ޤǻ��� ��pattern ���ץ�����
�ѥ�����λ���� --pattern ���ץ����ǹԤ��ޤ����ѥ�����ϥ��󥿡��ͤο�����ҤȤ�¿�����ꤷ�Ƥ�����Τ��ߥ��Ǥ���
���5��ο����Ͼ�2�夬���ֹ桢��3�夬�ȡ����ֹ��ɽ���Ƥ��ޤ������ֹ��dclclr ���ޥ�ɤ⤷���� cdclclr ���ޥ�ɡ��ȡ����ֹ�� dcltone ���ޥ�ɤ⤷���� cdcltone ���ޥ�ɤǻ��ȤǤ��ޤ���


  % gpview --levels -30,-20,-10,0,10,20,30 --pattern 30999,33999,36999,40999,70999,73999,76999,79999  T.jan.nc@T

���Ϸ�̡�
((:<center><IMG SRC="color.png" width=640 height=480></center>:))

������äƤ����ޤǤǤ��ޤ��������2��⤷����3��ο������ʤ��Ǥ��ޤ����������ϥȡ����ֹ�Ǥ���
  % gpview --levels -30,-20,-10,0,10,20,30 --pattern 51,53,200,201,202,203,204,205  T.jan.nc@T 

���Ϸ�̡�
((:<center><IMG SRC="sirokuro.png" width=640 height=480></center>:))

===�Ͽ���ơ���map ���ץ����, itr ���ץ�����
�Ͽ���Ƥˤ� --itr ���ץ�������Ƥμ����, --map ���ץ������Ѥ����Ͽ޾������ꤷ�ޤ���
itr �λ���ˤ����äƤ� DCL �ޥ˥奢���((<��������Ͽ����ˡ|URL:http://www.gfd-dennou.org/arch/ruby/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>))�򻲹ͤˤ��Ƥ���������
map�˻���Ǥ���Τ� coast_world, border_world, plate_world, state_usa, coast_japan, pref_japan ��6����Ǥ�  

������Ǥϥ��磻�ǿ�ˡ�������γ�������ŤͤƤ��ޤ���

  % gpview  --itr 12  --map coast_world T.jan.nc@T 

���Ϸ�̡�
((:<center><IMG SRC="map.png" width=640 height=480></center>:))

===�񤤤�������¸��������wsn ���ץ����������� D ������
--wsn���ץ����ǿޤν���������Ǥ��ޤ���
������Ǥϲ��̤Ǥ�����ϹԤ�줺�� PS �ե�����( dcl.ps )����������ޤ�����ʸ�ʤɤ˿ޤ�Ž��Ȥ��ϡ������㤬��������Ǥ��礦��
  % gpview  --wsn 2  T.jan.nc@T 

�ޤ������Τ褦�� wsn �� 1 ����ꤷ������ X �����褵��ޤ��ʥǥե���ȤǤ�������Ǥ��ˡ��ޤ��ǤƤ���Ȥ��� D �����򲡤����Ȥ� xwd �ե����뤬��������ޤ���
  % gpview  --wsn 1 T.jan.nc@T 

�ޤ������Τ褦�� wsn �� 4 ����ꤹ��� Gtk ���Ѥ������褵��ޤ����ޤ��ǤƤ���Ȥ��� D �����򲡤����Ȥ� png �ե����뤬��������ޤ��� web �ڡ����ʤɤ˿ޤ�Ž���դ��������ˤϤ�����ˡ���褤�Ǥ��礦���ºݤ��Υ��塼�ȥꥢ��˻Ȥ��Ƥ���ޤϤ�����ˡ����������ޤ�����
  % gpview  --wsn 4 T.jan.nc@T 

==1D������
===���ޤ�������
3�����ǡ�����2�Ĥμ����� fix ������ޤ�������դ������Ƥ���ޤ��������ն�ε����α�ľ�ץ�ե�����������Ƥߤޤ��礦��

  % gpview  T.jan.nc@T,lon=140,lat=40
���Ϸ�̡�
((:<center><IMG SRC="line.png" width=640 height=480></center>:))

===x����y�������촹�� ��exch ���ץ�����
���ä��γ����ȡ����������������ˤʤäƤ��ޤä����ᡢ��ľ�ץ�ե�����Ȥ��Ƥϸ��ˤ�����ΤˤʤäƤ��ޤ��ޤ�����
x����y�������촹���뤿��ˡ� --exch ���ץ���󤬤���ޤ���

  % gpview  --exch T.jan.nc@T,lon=140,lat=40
���Ϸ�̡�
((:<center><IMG SRC="line-exch.png" width=640 height=480></center>:))

���Υ��ץ����Ϥ�����2�����ޤǤ�ͭ���Ǥ���

===�Ĳ���λ��� ��aspect ���ץ�����
--aspect ���ץ�������ꤹ�뤳�ȤǽĲ�������Ǥ��ޤ������ꤷ�ʤ����ϽĲ��椬2�ˤʤäƤ��ޤ���

  % gpview  --exch --aspect 0.8 T.jan.nc@T,lon=140,lat=40

���Ϸ�̡�
((:<center><IMG SRC="aspect.png" width=640 height=480></center>:))

���Υ��ץ����Ϥ�����2�����ޤǤ�ͭ���Ǥ���

===�饤�󥤥�ǥå�������ꤹ�� ��index ���ץ�����
--index ���ץ����ǥ饤�󥤥�ǥå�������ꤷ�ƥ���դ��������뤳�Ȥ�Ǥ��ޤ���
�����������(2)������(5)�����������ޤ���
  % gpview --exch --index 25 T.jan.nc@T,lon=140,lat=40
���Ϸ�̡�
((:<center><IMG SRC="lineindex.png" width=640 height=480></center>:))

===�������ꤹ�� ��type ���ץ�����
--type ���ץ������������ꤷ�ƥ���դ��������뤳�Ȥ�Ǥ��ޤ���
�������������(2)�����������ޤ���
  % gpview --exch --type 2 T.jan.nc@T,lon=140,lat=40
���Ϸ�̡�
((:<center><IMG SRC="linetype.png" width=640 height=480></center>:))

===�ޤ�������դνŤͽ� ��overplot ���ץ�����
--overplot ���ץ�����Ȥ����Ȥ��ޤ����νŤͽ񤭤��ǽ�Ǥ�����Ǥ��̰�40�١����140�٤�150�٤β��ٱ�ľ�ץ�ե������Ťͽ񤭤��Ƥ��ޤ���
2�ܤ�����Ťͽ񤭤��뤿��� --overplot 2 ����ꤷ�ޤ���
  %  gpview --exch --overplot 2 T.jan.nc@T,lon=140,lat=40  T.jan.nc@T,lon=150,lat=40
���Ϸ�̡�
((:<center><IMG SRC="overplot.png" width=640 height=480></center>:))

===�ͤ��ϰϤ���� ��range ���ץ�����
���ä�����Ǥ��ͤ��ϰϤ����140�٤Υǡ������Ȥ˼�ư��������󥰤��줿��������150�٤Υ���դ��Ȥ���ϤߤǤƤ��ޤ��ޤ�����
�ͤ��ϰϻ����Ԥ������ --range ���ץ���󤬤���ޤ���
  % gpview --exch --overplot 2 --range -60:10 T.jan.nc@T,lon=140,lat=40 T.jan.nc@T,lon=150,lat=40
���Ϸ�̡�
((:<center><IMG SRC="range.png" width=640 height=480></center>:))

����ǤϤ߽Фʤ��ʤ�ޤ�����
���ε�ǽ���ޤ����Υ��˥᡼�����򤹤���ˤϤȤƤ�������ޤ���

===�ǡ����κǾ��������ͤ��Τꤿ����gpmaxmin ���ޥ�ɡ�
gpmaxmin ���ޥ�ɤ�Ȥ����Ȥ��ͤκǾ��������ͤ��Τ뤳�Ȥ��Ǥ��ޤ����㤨���̰�40�٤ΰ��ٱߤǤη���-���٥ǡ����κǾ��������ͤ��Τ�ˤϡ����Τ褦�ˤ��ޤ���
  % gpmaxmin T.jan.nc@T,lat=40
�����
  T.jan.nc@T,lat=40 : max=13.325665473938, at, 
       ...(total 1 points)...

  T.jan.nc@T,lat=40 : min=-62.9790420532227, at,  
       ...(total 1 points)...

��ɽ������ޤ����̰�40�٤η���-���٥ǡ����ˤ����ơ������ͤ�13�١��Ǿ��ͤ�-63�ٰ̤Ǥ��뤳�Ȥ��狼��ޤ���
���η�̤򻲹ͤˤ��ơ����ä�����Τ褦�� --range ���ץ������ͤ��ϰϤ���ꤷ�Ƥ�����ȡ���������ưŪ���Ѥ�뤳�Ȥʤ������ĥ���դ��Ȥ���Ϥ߽Ф뤳�Ȥʤ����˥᡼����󤬹Ԥ��ޤ���

  % gpview  --exch --range -65:15 --anim lon T.jan.nc@T,lat=40 

==�����ä�ʿ�ѿ� ��mean ���ץ�����
--mean ���ץ�����Ȥ��Ȥ��뼴������ʿ�Ѥ�Ȥä��ǡ����ǳ���񤯤��Ȥ��Ǥ��ޤ���T.jan.nc��Ĥ��äƤ����ä�1����Ӿ�ʿ�Ѳ��٤�񤤤Ƥߤޤ��礦��
  % gpview  --mean lon T.jan.nc@T 

���Ϸ�̡�
((:<center><IMG SRC="mean.png" width=640 height=480></center>:))

2������ʿ�Ѥ�ʤ�Τ��Ρ����������ˤ�ʿ�Ѥ��Ƥ��ޤ����Ȥ�Ǥ��ޤ���
  % gpview  --exch --mean lon,lat T.jan.nc@T 

���Ϸ�̡�
((:<center><IMG SRC="mean2.png" width=640 height=480></center>:))

==�ͥåȥ����Υǡ������ä�

OPeNDAP/DODS �б��� RubyNetCDF �����󥹥ȡ��뤵��Ƥ�����ˤϡ�DODS �����С���Υǡ�����Ȥä����褹�뤳�Ȥ��Ǥ��ޤ��������Ǥ�((<��Ǿ�ǡ��������С�|URL:http://davis.rish.kyoto-u.ac.jp/>))��Υǡ�����Ȥä����褷�Ƥߤޤ���

  % gpview http://davis-dods.rish.kyoto-u.ac.jp/cgi-bin/nph-dods/jmadata/gpv/netcdf/r1h/MSM-S/2006/0226.nc@r1h
���Ϸ�̡�
((:<center><IMG SRC="dods.png" width=640 height=480></center>:))


==��������
���Υ��塼�ȥꥢ��Ǥ� gpview �λȤ������ȡ��褯�Ȥ����ץ�����Ҳ𤷤ޤ��������塼�ȥꥢ��Ȥ������ϵհ�����ե���󥹤äݤ���ΤˤʤäƤ��ޤ��ޤ�����������
���Ƥ� gpview �Υ��ץ�����((<��ե���󥹥ޥ˥奢��|URL:http://dennou-k.gfd-dennou.org/arch/ruby/products/gphys/doc/gpview.html>))�˽񤫤�Ƥ��ޤ��Τǻ��ͤˤ��Ƥ���������
�ޤ���Ʊ�ͤ����Ƥ� --help ���ץ�����Ĥ��뤳�Ȥ�ɽ�����뤳�Ȥ��Ǥ��ޤ���

  % gpview  --help
