# GPhys, GGraph �`���[�g���A�� / �����̑���
#
# = ���� (�V�������̂͏ォ��ǋL)
#
# * 2012/02/29 �x�V����  ggraph_hop.rd ���番���C�啝�����E����

= GPhys, GGraph�`���[�g���A�� (����3)

== �����̑���

=== �f�B���N�g����t�@�C���̒��g��m��

���߂� irb �𗧂��グ (('ls')) �Ɠ��͂��Ă݂܂��傤�D

  $ irb_ggraph 
  irb(main):002:0> ls
  Directories:
    'air.2012-01.nc/'
    'hgt.2012-01.nc/'
    'ncep2.Jan.clim.1981-2010.nc/'
  => nil

NetCDF�t�@�C�����f�B���N�g���Ƃ��Ĉ����Ă��邱�Ƃ��킩��܂��D
�����ŁC(('ls 'air.2012-01.nc'')) �� (('ls_l 'air.2012-01.nc'')) 
�ȂǂƓ��͂��Ă݂܂��傤�D

  irb(main):005:0> ls 'air.2012-01.nc/'
  Data:
    'lon'
    'lat'
    'level'
    'time'
    'air'
  => nil
  irb(main):006:0> ls_l 'air.2012-01.nc/'
  Data:
    lon	[lon=144]	'Longitude'	(degrees_east)
    lat	[lat=73]	'Latitude'	(degrees_north)
    level	[level=17]	'Level'	(millibar)
    time	[time=31]	'Time'	(hours since 1-1-1 00:00:0.0)
    air	[lon=144,lat=73,level=17,time=31]	'mean Daily Air temperature'	(degK)
  => nil
  irb(main):007:0> ls_l 'ncep2.Jan.clim.1981-2010.nc/'
  Data:
    lon	[lon=144]	'Longitude'	(degrees_east)
    lat	[lat=73]	'Latitude'	(degrees_north)
    level	[level=17]	'Level'	(millibar)
    air	[lon=144,lat=73,level=17]	'Monthly Air Temperature on Pressure Levels'	(degK)
    hgt	[lon=144,lat=73,level=17]	'Monthly Geopotential Heights on Pressure Levels'	(m)
    mslp	[lon=144,lat=73]	'Monthly Mean Sea Level Pressure'	(Pascals)
    uwnd	[lon=144,lat=73,level=17]	'Monthly U-wind on Pressure Levels'	(m/s)
    vwnd	[lon=144,lat=73,level=17]	'Monthly V-wind on Pressure Levels'	(m/s)
  => nil

����� (ncdump ���g��Ȃ��Ă��R���p�N�g�Ȍ`��),
�Ⴆ�� air.2012-01.nc ���̕ϐ� air ��
lon, lat, level, time �Ƃ������O�̎��������� 4�����̕ϐ��ł��邱�Ƃ�, 
�e�����̒��� (144, 73, 17, 31) ���킩��܂��D

ls �� ls_l �� irb_ggraph �œǂݍ���
((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) 
�ɒ�`����Ă��܂��D���� cd (�J�����g�f�B���N�g���̕ύX) �� pwd (�J�����g�f�B���N�g���̕\��)
������܂��D


=== �����Ȓf��

���x�͓����ϋC���� tmp �Ƃ����ϐ����ŊJ���Ă݂܂��傤
�i�ϐ����͍D���ɂ����܂����C�������ł͂��߂Ă��������D-- 
�啶���Ŏn�܂�̂́u�萔�v�ƂȂ�܂��j�D

  irb(main):001:0> tmp = gpopen 'air.2012-01.nc/air'
  => <GPhys grid=<4D grid <axis pos=<'lon' in '//home/username/air.2012-01.nc'  sfloat[144]>>
          <axis pos=<'lat' in '/home/username/air.2012-01.nc'  sfloat[73]>>
          <axis pos=<'level' in '/home/username/air.2012-01.nc'  sfloat[17]>>
          <axis pos=<'time' in '/home/username/air.2012-01.nc'  float[31]>>>
     data=<'air' in '/home/username/air.2012-01.nc'  sint[144, 73, 17, 31]>>

�n�}���e�̐ݒ�����C������4�Ԗڂ́u���x�v(700 hPa�̋C����)�C
�O����10�Ԗڂ̎���(1��10���̓����ϒl)�̒f�ʂ�\�����Ă݂܂��D

  irb(main):002:0> set_fig "itr"=>32          # �������ʐ}�@
  irb(main):003:0> set_map "coast_world"=>true
  irb(main):006:0> tone_and_contour tmp[true,true,3,9]

�����ł� (('tmp')) �� (('[true,true,3,9]')) �����Ă��܂��D
���̎l�p���ʂ́C�Y���ɂ�镔���u�z��v�̎w��ł��D
��1,2������ (('true')) �͑S�I����\���܂��D
Ruby �̔z��̓Y���̓[������n�܂�܂��̂ŁC
��3, 4������ 4, 10 �Ԃ̗v�f���w�肷��̂ɂ��ꂼ��i1�������āj 3, 9 �Ǝw�肷��̂ł��D

���ʁF
((:<a href="img/ggraph_step_01.png">:))
((<"IMG:img/ggraph_step_01_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))


���x�͉����f�ʂ������Ă݂܂��傤�D
�C�����W�̕\���͑ΐ��X�P�[�����悢�ł��̂ŁC
���e�@��2�ԁi�c���̂ݑΐ����W�j���Ƃ�܂�
(�Q�l�F((<���W�n�̎��|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>)))�D
����1��10���̌o�x140�x�ł̒f�ʂ�\�����܂��傤�D���W�l�ł̎w��ɂ͎��̗p��
(('cut')) ���g���܂��D

  irb(main):005:0> set_fig "itr"=>2
  irb(main):006:0> tone_and_contour tmp[false,9].cut("lon"=>140)

�i�u�h�b�g�łȂ�����Ăǂ��������ƁH�v�Ǝv��ꂽ���C����ɂ��Ă͎��̏͂Ő������܂��D�j

���ʁF
((:<a href="img/ggraph_step_02.png">:))
((<"IMG:img/ggraph_step_02_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))


=== �d�˂���

�}�͏d�˂������邱�Ƃ��ł��܂�. �������s���Ă݂Ă��������D

  irb(main):007:0> tone tmp[true,true,0,10] - tmp[true,true,0,8]  # �ŉ��w(1000 hPa)�ł�1��11����9���̍���F��
  irb(main):009:0> color_bar                                     # �J���[�o�[
  irb(main):008:0> contour tmp[true,true,0,9], false             # 1��10����1000 hPa�C�����R���^�[��

GGraph �̕`�惁�\�b�h�̓I�v�V�����ő�2������^���邱�Ƃ��ł��܂��D
��̗��3�s�ڂ̂悤�� false ��^����Ə㏑���ɂȂ�܂��D
true (����l) ��^������ȗ������ꍇ�̓y�[�W��t���[�������߂ĐV���ɕ`�悵�܂��D

���ʁF
((:<a href="img/ggraph_step_03.png">:))
((<"IMG:img/ggraph_step_03_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

���ܓx�œ��������g�ł��Ă���Ƃ���ŋC�����傫���������Ă��邱�Ƃ��킩��܂��D

=== �`��I�v�V���� (2)

�}�̃J�X�^�}�C�Y�ł���, 
�`�惁�\�b�h�ɂ���Ă�, set_fig �̂悤�ȃ��\�b�h�łȂ�, 
�`�惁�\�b�h�̈����ɂ���ĕύX���邱�Ƃ��ł��܂�.
�i�Ȃ����̂悤�Ȑ؂蕪�������邩�m�肽����Ύ��̐߂��������������D�j

  irb(main):008:0> tone tmp, true, "interval"=>5, "max"=>300, "min"=>230, "color_bar"=>true, "title"=>"1000 hPa T"

���̂Ƃ�, �������i�㏑���Ɋւ���true�܂���false�j�͏ȗ��ł��Ȃ��̂Œ��ӂ��Ă�������.

���ʁF
((:<a href="img/ggraph_step_04.png">:))
((<"IMG:img/ggraph_step_04_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

�ǂ̂悤�ȃI�v�V���������邩�ɂ��ẮC
GGraph �� ���t�@�����X�}�j���A��
(((<�p��|URL:http://ruby.gfd-dennou.org/products/gphys/doc/ggraph.html>)),
((<���{���|URL:http://w.livedoor.jp/gphys/d/module%20NumRu%3a%3aGGraph>)))
���Q�Ƃ��Ă��������D

�`��I�v�V������ help �I�v�V�������g���đΘb�I�ɒ��ׂ邱�Ƃ��ł��܂��D

 irb(main):001:0> tone nil, true, "help"=>true
  *** MESSAGE (SWDOPN) ***  GRPH1 : STARTED / IWS =  1.                         
 << Description of options >>
   option name	default value	# description:
   "title"	nil	# Title of the figure(if nil, internally
                         # determined)
   "annotate"	true	# if false, do not put texts on the right
                         # margin even when newframe==true
   "ltone"	true	# Same as udpack parameter ltone
   "auto"	false	# Swith DCL.uetone and DCL.uetonf depending on the
                         # data size
   "tonf"	false	# Use DCL.uetonf instead of DCL.uetone
   "tonb"	false	# Use DCL.uetonb instead of DCL.uetone
   "tonc"	false	# Use DCL.uetonc instead of DCL.uetone
   "clr_min"	nil	# if an integer (in 10..99) is specified, used as
                         # the color number for the minimum data values.
                         # (the same can be done by setting the uepack
                         # parameter "icolor1")
   "clr_max"	nil	# if an integer (in 10..99) is specified, used as
                         # the color number for the maximum data values.
                         # (the same can be done by setting the uepack
                         # parameter "icolor2")
   "map_axes"	false	# [USE IT ONLY WHEN itr=10 (cylindrical)] If
                         # true, draws axes by temprarilly switching to
                         # itr=1 and calling GGraph::axes.
   "keep"	false	# Use the tone levels and patterns used previously
   "color_bar"	false	# Add a color bar: THIS IS ONLY FOR QUICK
                         # LOOK. Use the GGraph::color_bar method explicitly
                         # for full option control
   "min"	nil	# minimum tone level
   "max"	nil	# maximum tone level
   "nlev"	nil	# number of levels
   "interval"	nil	# contour interval
   "help"	false	# show help message if true
   "log"	nil	# approximately log-scaled levels (by using
                         # DCLExt::quasi_log_levels)
   "log_cycle"	3	# (if log) number of levels in one-order (1 or 2
                         # or 3)
   "levels"	nil	# tone levels  (Array/NArray of Numeric). Works
                         # together with patterns
   "patterns"	nil	# tone patters (Array/NArray of Numeric). Works
                         # together with levels
   "exchange"	false	# whether to exchange x and y axes
   "transpose"	false	# if true, exchange x and y axes
   "xintv"	1	# interval of data sampling in x
   "yintv"	1	# interval of data sampling in y
   "xcoord"	nil	# Name of the coordinate variable for x-axis
   "ycoord"	nil	# Name of the coordinate variable for y-axis
   "slice"	nil	# An Array to be pathed to the GPhys#[] method to
                         # subset the data before plotting (order applied:
                         # slice -> cut -> mean)
   "cut"	nil	# An Array or Hash to be pathed to the GPhys#cut
                         # method to subset the data before plotting (order
                         # applied: slice -> cut -> mean)
   "mean"	nil	# An Array to be pathed to the GPhys#mean method to
                         # take mean of the data before plotting (order
                         # applied: slice -> cut -> mean)

�I�v�V�����̑O�ɂ͏��2�������K�v�Ȃ��Ƃɒ��ӂ��Ă��������D
�������͒ʏ�� GPhys �f�[�^�ł����Chelp �I�v�V�������g���ꍇ�_�~�[�ō\���܂���D
���̗�ł� nil (�Ȃ�ł��Ȃ��Ƃ�����`�ς݃I�u�W�F�N�g) ���w�肵�܂����D
��2�����͏�� true �܂��� false ��^����K�v������܂��D


=== GGraph �̍\���ƃI�v�V�����ɂ��� �i��F�Ƃ΂��č\���܂���j

���� 
(('set_fig')) �Ŏw�肷��I�v�V�����́C(('fig')) �Ƃ������\�b�h�̃I�v�V�����Ȃ̂ł�
�i���l�� (('set_map')) �� (('map')) �̃I�v�V�����w��C�܂��o�Ă܂���
(('set_axes')) �Ƃ��� (('axes')) �̃I�v�V�����w�������܂��j�D
(('contour')) ���͓����ŕK�v�ɉ����� (('fig')) ���ĂԂ̂ł����C(('contour')) 
�ɂ� (('fig')) �̃I�v�V�����͒��ڂ͓n���Ȃ��̂őO������ (('set_fig')) 
�Ŏw�肷��Ƃ����\���ɂȂ��Ă܂��D�����̐؂蕪���́C

* (('fig')) : �}�̊�{�\�������߂�
* (('axes')) : �}�̏c���������i��G
* (('map')) : �n�}���e�Ɋւ��邱�Ƃ��i��

�ł��D������

* (('contour')) �� (('tone')) �Ȃǌʂ̕`�惁�\�b�h�̃I�v�V�����͂́C
  ���ꂼ��̕`��Ɋւ�邱�Ƃ����߂�

�Ƃ����悤�ɂȂ��Ă��܂��D�O�q�� title �Ȃǂ̃I�v�V������ 
(('fig')) �� (('axes')) �Ɋւ�肻���Ɏv���܂����C
�z�Ɏw�肳��Ȃ��ꍇ�̃^�C�g���͕`��Ώۂ̖��O����Ƃ�܂��̂�
�ʂ̕`�惁�\�b�h�̃I�v�V�����ɂȂ��Ă��܂��D


((* 2012-02-29 �����܂ŕҏW�D�x�V�� *))

=== �x�N�g���`��


���x��1���̋C��l (1981�N����2010�N�܂ł�30�N���ϒl) ��ǂݍ���ł݂܂��傤�D

  irb(main):008:0> tc = gpopen 'ncep2.Jan.clim.1981-2010.nc/air'    # �C��
  irb(main):009:0> zc = gpopen 'ncep2.Jan.clim.1981-2010.nc/hgt'    # ���x
  irb(main):010:0> uc = gpopen 'ncep2.Jan.clim.1981-2010.nc/uwnd'   # ������
  irb(main):011:0> vc = gpopen 'ncep2.Jan.clim.1981-2010.nc/vwnd'   # ��k��




vector uc.cut("level"=>500), vc.cut("level"=>500)


==== ���K���

(1) 'interval' ���g���ăg�[���E�R���^�[�̊Ԋu��ݒ肵�Ă݂܂��傤. 
    ���̂Ƃ�, �g�[���̊Ԋu�ƃR���^�[�̊Ԋu�͈قȂ�l�ɂ�,
    ���d�˂������Ă݂܂��傤. 

# �͂܂肻���Ȃ̂ň�U�R�����g�A�E�g.  �]�T������Ƃ��Ɍ�����.
#(2) (�����) �`�惁�\�b�h�̑�O�����ȍ~�͕����������Ƃ��o���܂�.
#    GPhys ���t�@�����X�}�j���A��
#    (
#    ((<"�p��"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
#    ((<"���{���"|URL:http://w.livedoor.jp/gphys/>))
#    ) ���Q�l��, (1) �̌��ʂɉ�����
#    �D���ȍő�l�E�ŏ��l��ݒ肵�Ă݂܂��傤.
#    (�q���g: �����ł� GGraph �Ō�����, �s������̃y�[�W�� "maximum"
#    (���{��łȂ�"�ő�") �Ō��������...)

# �������Ԃ̃f�[�^������ꍇ�͂����ŉ����ł�����.

=== �o�͐��ύX����

����, �D���Ȑ}��`�����Ƃ����, 
�}���t�@�C���ɕۑ����Ă݂܂��傤.
���̏o�͐�� X �ɂȂ��Ă���̂�, 
postscript �t�@�C���ɏo�͂���悤�ɐ؂�ւ��Ă݂܂�. 

  DCL.grcls  # ���̑������

���̂܂܂ł̓v�����v�g���A���Ă��Ȃ��̂�, 
�}���N���b�N���ď����܂�.

�o�͐�� postscript �t�@�C���ɕύX���ďo�͂��܂�.

  DCL.gropn(2)   # �o�͑��u�ԍ��� 2 �� (postscript �t�@�C��) �ɕύX. �t�@�C���I�[�v���ɑ���.
  tone(gp)       # �o��. ���̒ʂ�łȂ��Ă��悢.
  DCL.grcls      # ���u�����. �t�@�C���N���[�Y�ɑ���.

�t�@�C��������O��, �ŏ��̏�Ԃɖ߂����K�����Ă����܂��傤.

  DCL.gropn(1)  # X �͏o�͑��u 1 ��.

�Ƃ��܂�. irb �𗧂��グ�����̂悤��, ������ʂ�����܂�.

�t�@�C�����m�F���邽�߂Ɉ�U irb ���甲���܂�.
�ʂ̒[�����グ�Ă��\���܂���.

  exit

�J�����g�f�B���N�g���� ls ����� dcl.ps �Ƃ����t�@�C�����ł��Ă���͂��ł�.

  $ ls

�ǂ������G���ł��Ă��邩�m�F���Ă݂܂��傤.

  $ gv dcl.ps

�o�͐�t�@�C���� dcl.ps �͌Œ�Ȃ̂�, 
�ԈႦ�ď㏑�����Ȃ��悤�ɍD���Ȗ��O�ɕς��Ă����܂��傤.

  $ mv dcl.ps practice01.ps


=== �h�L�������g�ɂ���

�����ł͗��K�̂��߂Ƀ��t�@�����X�}�j���A�����Q�Ƃ��܂�����, 
��r�I�P���ȏꍇ��
((<"GPhys/GGraph �`�[�g�V�[�g"|URL:http://davis.gfd-dennou.org/rubygadgets/ja/?%28Others%29+GPhys%2FGGraph+%A5%C1%A1%BC%A5%C8%A5%B7%A1%BC%A5%C8>))
���Q�Ƃ��Ă��悢�ł��傤.

�܂�, GGraph �ׂ͍��������͋��ł��̂�,
������肽���ꍇ�͂��̉�������
((<"RubyDCL"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/>))
���Q�l�ɂ��܂��傤.

=== �Q�l����

* ((<"GPhys/GGraph �`�[�g�V�[�g"|URL:http://davis.gfd-dennou.org/rubygadgets/ja/?%28Others%29+GPhys%2FGGraph+%A5%C1%A1%BC%A5%C8%A5%B7%A1%BC%A5%C8>))
* GPhys ���t�@�����X�}�j���A��
  (
  ((<"�p��"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
  ((<"���{���"|URL:http://w.livedoor.jp/gphys/>))
  )
  * ���̒��� GGraph �� ���t�@�����X�}�j���A��
    (((<�p��|URL:http://ruby.gfd-dennou.org/products/gphys/doc/ggraph.html>)),
    ((<���{���|URL:http://w.livedoor.jp/gphys/d/module%20NumRu%3a%3aGGraph>)))

* ((<"RubyDCL �h�L�������g"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/>))
* ((<"���낢��Ȓn�}���e�@"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (RubyDCL �h�L�������g��)
* ((<"DCL colormaps"|URL:http://www.gfd-dennou.org/library/dcl/dcl-5.4.2/src/env1/colormap/colormap_gallery.html>)) (DCL �h�L�������g��)

