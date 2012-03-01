# GPhys, GGraph �`���[�g���A�� / �����̑���
#
# = ���� (�V�������̂͏ォ��ǋL)
#
# * 2012/02/29 �x�V����  ggraph_hop.rd ���番���C�啝�����E����

= GPhys, GGraph�`���[�g���A�� (����3)

== �����̑���

�������� irb ���g���Ă��G�`�����܂��D
�����Ȓf�ʂ�Y�[���A�b�v�C�ȒP�ȑ���i���ρC�����Z���j��̌����܂��D
�`��̃I�v�V�������Љ�܂����C�܂����Ă���̍ו��ɂ͂�����炸�C���]�̊G��f�����`�����Ƃ��d�����܂��D

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


=== �`��I�v�V���� (2)

�}�̃J�X�^�}�C�Y�ł���, 
�`�惁�\�b�h�ɂ���Ă�, set_fig �̂悤�ȃ��\�b�h�łȂ�, 
�`�惁�\�b�h�̈����ɂ���ĕύX���邱�Ƃ��ł��܂�.
�i�������Ȃ����̂悤�Ȑ؂蕪�������邩�m�肽����Ύ��X�߂��������������D�j

  irb(main):007:0> set_fig "itr"=>32          # �Ăѐ������ʐ}�@��
  irb(main):008:0> tone tmp, true, "interval"=>5, "max"=>300, "min"=>230, "color_bar"=>true

tone �̈����ŕK�{�Ȃ͍̂ŏ��̈��(GPhys�I�u�W�F�N�g)�ł����C
�I�v�V�����ł���Ɉ�����^���邱�Ƃ��ł��܂��D
��2����(�����ł� true)�͐}�̏d�˂����Ɋւ���w��ŁC((<����|�d�˂���>))�ŉ�����܂��D
�Ƃ肠�����͂��܂��Ȃ��Ƃ��� true ���w�肷����񂾂Ǝv���Ă����Ă��������D
��3�������炪�I�v�V�����w��ł��i��ł��܂����C���� (('"interval"=>5', ')) 
�ȍ~�͂��ׂĂ܂Ƃ߂đ�3�����Ȃ̂ł����C�ɂ��Ȃ����Ƃɂ��܂��傤�j�D
�I�v�V������ interval, max, min �͐F�t���̒i�K�̊Ԋu�C�ő�l�C�ŏ��l�̎w��ł��D
color_bar �͂���܂� tone �̂��ƕʃ��\�b�h�Ƃ��ČĂ�ł܂������C���̂悤�ɃI�v�V�����ɂ��ł��܂��D

���ʁF
((:<a href="img/ggraph_step_03.png">:))
((<"IMG:img/ggraph_step_03_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

interval �̑���� nlev �ł��悻�̃��x�������w�肷�邱�Ƃ��ł��܂��F

  irb(main):008:0> tone tmp, true, "nlev"=>20   # ���x�������20�ɁD�Ԋu�͂��C���D


�Ƃ������I�v�V�������w�肷��Ƃ���, ��2�����͏ȗ��ł��Ȃ��̂Œ��ӂ��Ă�������.
�Y���Ƃ���ȃG���[���b�Z�[�W���ł܂��D

  irb(main):010:0> tone tmp, "interval"=>5   # ��2������Y��ăG���[�ɂȂ��
   *** MESSAGE (SWDOPN) ***  GRPH1 : STARTED / IWS =  1.                         
  ArgumentError: 2nd arg (newframe) must be true or false
          from /usr/lib/ruby/1.8/numru/ggraph.rb:2619:in `tone'
          from (irb):10
          from /usr/bin/irb:12:in `<main>'


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
   ..(�㗪)..

((:<div class=likepre>:))
((<((*�itone�̃I�v�V�����̑S�̂��݂�j*))|URL:ggraph_step_sub1.htm>))
((:</div>:))

�I�v�V�����̑O�ɂ͏��2�������K�v�Ȃ��Ƃɒ��ӂ��Ă��������D
�������͒ʏ�� GPhys �f�[�^�ł����Chelp �I�v�V�������g���ꍇ�_�~�[�ō\���܂���D
���̗�ł� nil (�Ȃ�ł��Ȃ��Ƃ�����`�ς݃I�u�W�F�N�g) ���w�肵�܂����D
��2�����͏�� true �܂��� false ��^����K�v������܂��D

���ɓ��{�t�߂��g��\�����Ă݂܂��傤�D�n�}���e�@�͐����~���}�@�ɐ؂�ւ��C
�C�ݐ��f�[�^�͑S�����J�o�[���邯�Ǖ���\�����r�� coast_world ����C
���{�t�߂��ׂ����J�o�[���� coast_japan �ɐ؂�ւ��āC
�o�x�ܓx�͈͂��i���ĕ\�����܂��D

  irb(main):004:0> set_fig "itr"=>10          # �����~���}�@
  irb(main):005:0> set_map "coast_japan"=>true, "coast_world"=>false
  irb(main):009:0> tone tmp.cut("lon"=>120..150,"lat"=>20..50)
  irb(main):009:0> color_bar

((:<a href="img/ggraph_step_04.png">:))
((<"IMG:img/ggraph_step_04_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

==== ���K���

(1) 'interval' ���g���ăg�[���E�R���^�[�̊Ԋu��ݒ肵�Ă݂܂��傤. 
    ���̂Ƃ�, �g�[���̊Ԋu�ƃR���^�[�̊Ԋu�͈قȂ�l�ɂ�,
    ���d�˂������Ă݂܂��傤. 

(2) �����~���}�@ ("itr"=>10) ���g����1��10���� 500 hPa �̋C����,
    �o�x�͈� 90�`270 �͈̔�(�ܓx�͓�ɂ���k�ɂ܂�)�ŕ`���Ă݂܂��傤
    �i�c���Ƃ�180�x�͈̔͂Ȃ̂Ő����`�̐}�ɂȂ�͂��ł��j�D

=== �d�˂���

�}�͏d�˂������邱�Ƃ��ł��܂�. �������s���Ă݂Ă��������D

  irb(main):007:0> tone tmp[true,true,0,10] - tmp[true,true,0,9], true, "title"=>"T & its tendency"
                   #���ŉ��w(1000 hPa)�ł�1��11����10���̍���F��
  irb(main):009:0> color_bar                                     # �J���[�o�[
  irb(main):008:0> contour tmp[true,true,0,9], false             # 1��10����1000 hPa�̋C�����R���^�[��

GGraph �̕`�惁�\�b�h�̓I�v�V�����ő�2������^���邱�Ƃ��ł��܂��D
��̗��3�s�ڂ̂悤�� false ��^����Ƃ���܂ł̕`��̏�ɏd�˂ĊG��������܂��D
���̍ہC���W����^�C�g���͍ĕ`�悵�܂���D
����Ctrue (����l)��^������ȗ������ꍇ�̓y�[�W��t���[�������߂ĐV���ɕ`�悵�܂��D
�}�̃^�C�g���͍ŏ��̕`�掞 (tone ...) �ɏ������̂ŁC�������}������Ă����̂ł����,
��ł킩��₷���^�C�g��������Ƃ����ł��傤 ("T & its tendency" �Ƃ��܂���)�D

���ʁF
((:<a href="img/ggraph_step_05.png">:))
((<"IMG:img/ggraph_step_05_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

���ܓx�œ��������g�ł��Ă���Ƃ���ŋC�����傫���������Ă��邱�Ƃ��킩��܂��D

==== ���K���

(1) tone �� contour �̗l�X�ȏd�ˏ���������Ă݂܂��傤�D

=== GGraph�̍\���ƃI�v�V�����i��F�Ƃ΂��č\���܂���j

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

�����ň�U irb �̃Z�b�V���������؂肵�܂��傤�D
�����������͓��e�͕ۑ����܂��傤�i((<�Q�l|URL:ggraph_hop.htm>))�j�D

  irb(main):022:0> history_save
  irb(main):022:0> exit


=== �x�N�g���`��

�܂� irb �𗧂��グ�܂��傤�D

  $ irb_ggraph 

���x��1���̋C��l (1981�N����2010�N�܂ł�30�N���ϒl) ��ǂݍ��݂܂��D

  irb(main):008:0> tc = gpopen 'ncep2.Jan.clim.1981-2010.nc/air'    # �C��
  irb(main):009:0> zc = gpopen 'ncep2.Jan.clim.1981-2010.nc/hgt'    # ���x
  irb(main):010:0> uc = gpopen 'ncep2.Jan.clim.1981-2010.nc/uwnd'   # ������
  irb(main):011:0> vc = gpopen 'ncep2.Jan.clim.1981-2010.nc/vwnd'   # ��k��

�����~���}�@(�ԍ�10)��850 hPa�̑S���̕�������ŕ\�����܂��D
�����̐}�ɂȂ�̂ŁC���� "viewport" ���L���܂��傤
�i�Q�l�FDCL�̃}�j���A����((<���K�ϊ��p�����^|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node9.html>))�j�D

  irb(main):036:0> set_fig "itr"=>10, "viewport"=>[0.05, 0.85, 0.3, 0.7]
  irb(main):037:0> set_map "coast_world"=>true
  irb(main):038:0> p = 850
  irb(main):039:0> vector uc.cut("level"=>p), vc.cut("level"=>p), true, "xintv"=>4, "yintv"=>4, "unit"=>true

�����ł͋��ʂ̋C���ʂ���邽�ߕϐ����g���܂����D(('p=850')) �Ƃ�����ŁCu, v 
���ʂ� cut ���Ă��܂��D�i�q�_���������̂ŁC�I�v�V������ "xintv", "yintv" �ŊԈ����C
�I�v�V���� "unit" �ŗ��O�ɖ��̒����X�P�[����\�����邱�Ƃ��w�肵�Ă��܂��D
�Ȃ��C"unit" �́C�������� "unit_vect" �̗��ł��D
((* GGraph �̃I�v�V�����́C����̂Ȃ��͈͂Ō����ȗ��ł��܂��D*))

���ʁF
((:<a href="img/ggraph_step_06.png">:))
((<"IMG:img/ggraph_step_06_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

���x�͓����ʍ��x��F�ł����āC���̏�ɐ�قǂ̃x�N�g�����d�˂Ă݂܂��傤�D

  irb(main):025:0> tone zc.cut("level"=>p)
  irb(main):026:0> vector uc.cut("level"=>p), vc.cut("level"=>p), false, "xintv"=>4, "yintv"=>4, "unit"=>true
  irb(main):027:0> color_bar "vcent"=>0.5, "vlen"=>0.25

�����łׂ͍����H�v������܂��D�f�t�H���g�ɔC���ăJ���[�o�[��������,
�x�N�g���X�P�[���\���Əd�Ȃ��Ă��܂��܂��̂ŁC
color_bar ���\�b�h�̃I�v�V�����Œ��S�̈ʒu�ƒ������w�肵�Ă��܂�
�i0�`1�ŃX�P�[�����ꂽ�r���[�|�[�g��Y���ɂ��āj�D

���ʁF
((:<a href="img/ggraph_step_07.png">:))
((<"IMG:img/ggraph_step_07_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

���͂����������x�̓��l���ɉ����Ă܂����i�n�t���j�����X�傫������������Ă��܂��D

==== �ȉ�����

���x�͏��� 200 hPa �C���ʂŕ`���Ă݂܂��D
�n�}���e�̓|�[���[�X�e���I�ɕς��Ă݂܂��傤�i����Ńx�N�g�����\���ł���̂� GPhys 1.2 �ȍ~�Ɍ����܂��j�D
���̐}�@�ł͓�ɂ��������ɓ��e����܂��D�ܓx�͈͂�����Ȃ��i�ł����Ă��܂��j�Ɖ�ʈ�ʂɕ\������܂��D�����Ȃ�Ȃ��悤�CDCL �ŃN���b�s���O���w�肵�܂�
�i�Q�l�FDCL�̃}�j���A����((<��ԂƃN���b�s���O|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node22.html>))�j�D

  irb(main):006:0> p=200
  irb(main):006:0> DCL.sgpset("lclip", true)
  irb(main):013:0> set_fig "itr"=>31, "viewport"=>[0.12, 0.82, 0.15, 0.85]
  irb(main):017:0> tone zc.cut("level"=>p), true, "title"=>"Z & wind"
  irb(main):018:0> vector uc.cut("level"=>p), vc.cut("level"=>p), false, "xint"=>3, "yint"=>3, "factor"=>2, "unit"=>true 
  irb(main):027:0> color_bar "vcent"=>0.5

���ʁF
((:<a href="img/ggraph_step_08.png">:))
((<"IMG:img/ggraph_step_08_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

vector �̃I�v�V���� factor �́C���C���ł��܂���̒�����萔�{���܂��i�����j�D
���C���ł͖�󓯎m���قڏd�Ȃ�Ȃ��X�P�[���ɒ�������܂����C2�{�����̂Ŏ኱�d�Ȃ��Ă܂��D

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

=== ���̏͂ň��p��������

* DCL : ((<���W�n�̎��|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>))

* GGraph �� ���t�@�����X�}�j���A�� (GPhys �̃}�j���A����)
  (((<�p��|URL:http://ruby.gfd-dennou.org/products/gphys/doc/ggraph.html>)),
  ((<���{���|URL:http://w.livedoor.jp/gphys/d/module%20NumRu%3a%3aGGraph>)))

=== ����Ȃ�Q�l����

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

