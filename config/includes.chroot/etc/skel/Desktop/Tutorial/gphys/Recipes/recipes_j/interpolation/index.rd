= ��ԁC���W�ϊ�

==== �x�V����  �ŏI�X�V2010/03/06

GPhys �ł͑�������((*���`���*))�ł��܂��D��ԃ��\�b�h�� interpolate
�Ƃ������O�ŁC�R�A�̕�����C�ɂ��g�����C�u�����Ƃ��Ď�������Ă��܂��D
���̃��\�b�h�̏�ɁC�}�E�X�ł̐؂�o����i�q�_���킹�̃��\�b�h���񋟂���Ă܂��D

  �{�e�̑Ή��o�[�W�����F GPhys 1.0.0�ȍ~

=== �ڎ�

* ((<���(���O�})>))
  * ((<1�_�ւ̓��}>))
  * ((<�i�q�ɓ��}�^�΂߂ɐ؂�>))
* ((<�}�E�X�N���b�N�Œf�ʐ؂�o��>))
* ((<���W�ϊ�>))
  * ((<�C�ӂ�1�����̕ϊ��i�������W�ϊ��C���ʍ��W�̓����Ȃǁj>))
  * ((<2�������W�ϊ�>))
* ((<�i�q���킹>))
* ((<����ɂ��킵��>))
  * ((<�\�[�X�t���̗��p��>))
  * ((<��Ԃ̎d�l>))

== ���(���O�})

((*����*)): 
NCEP �ĉ�͂̋C���f�[�^ ((<air.2010.nc|URL:air.2010.nc>)) 
((-���̃f�[�^��((<NOAA�̃T�C�g|URL:http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html>))����擾���܂����D�{�t�@�C���ɂ�1-2���̃f�[�^�̂ݓ����Ă܂��D-)) 
��K���ȍ�ƃf�B���N�g���Ƀ_�E�����[�h���C������ (('cd')) ���܂��D
##�܂��Cirb�p���ʃX�^�[�g�A�b�v�t�@�C��
##((<irbrc_ggraph.rb|URL:../irbrc_ggraph.rb>)) ���z�[���f�B���N�g�� (('~')) 
##�܂��͌��݂̃f�B���N�g���Ƀ_�E�����[�h���܂��D�ȉ��ł̓z�[���f�B���N�g���ɒu�������̂Ƃ��Ęb��i�߂܂��D

=== 1�_�ւ̓��}

��ԂɎg�� GPhys �̃��\�b�h�� (({interpolate})) �ł��D�D�y(141E,43N)�̋C���̍��x���Ԓf�ʂ��Ԃŋ��߂Đ}�����Ă݂܂��傤�D

�T���v���v���O���� ((<interpo1_sapporo.rb|URL:interpo1_sapporo.rb>)):
  require "numru/ggraph"
  include NumRu

  #< open data / select a date for conciseness >

  temp = GPhys::IO.open("air.2010.nc","air").cut(
            "time"=>Date.parse("2010-01-04")..Date.parse("2010-01-9"))

  #< prepare new coordinates and interpolate >

  lon = VArray.new(NArray[141.0],{"units"=>"degree_east"},"lon")
  lat = VArray.new(NArray[43.0],{"units"=>"degree_north"},"lat")
  tsapporo = temp.interpolate(lon,lat)[0,0,false]

  #< graphics >

  iws = (ARGV[0] || 1).to_i
  DCL.swpset('ldump',true) if iws==4
  DCL.swpset('iwidth',700)
  DCL.swpset('iheight',700)
  DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
  DCL.glpset('lmiss',true)
  DCL.gropn(iws)
  GGraph::tone_and_contour tsapporo, true, "exchange"=>true
  DCL.grcls

���s
  ruby interpo1_sapporo.rb

���s����((<(full size)|URL:interpo1_sapporo.png>)) :
((<"IMG:interpo1_sapporo_th.jpg">))

(('interpolate')) ���\�b�h�ł́C��Ԍ�̍��W�������Ƃ��ė^���܂��D�����ł́C
(('"lon"')), (('"lat"')) ������ł��D���ꂼ�꒷��1�̍��W�� (((<VArray�N���X|URL:http://ruby.gfd-dennou.org/products/gphys/doc/varray.html>))) �ɂȂ��Ă܂��̂ŁC
�ܓx�o�x�̈�_���w�肷�邱�ƂɂȂ�܂��D���� 1 �ł��C�����Ƃ��Ďc��4�����̂܂܂ł��̂ŁC
interpolate ����� (({[0,0,false]})) ��K�p����(0�ł��̎����̍ŏ��̗v�f���w�肵��)�Q���������Ă��܂��D
�Ȃ��C(('interpolate')) �͍��W���̖��O�őΉ��𔻒f���܂��̂ŁC���� ("lon", "lat") �̖��t���͕K�{�ł��D
����C�P�� ((('"degree_east"')) ��) �͐H���Ⴆ�Ζ��������̂ŁC
�^���Ȃ��Ă��\���܂���(�x���͕\������܂�)�D�������C"km" �� "m" �� "days since 2010-01-01" 
�� "hours since 1900-01-01" �̂悤��((*���Z�\�ȒP�ʂɂ��Ă͊��Z*))���܂��D

�Ȃ��C��_���w�肷��ꍇ�C���O�ƒl�̑g�� Hash �Ŏw�肷�邱�Ƃ��ł��܂��D
�������CGPhys 1.0.0 �ł́CHash �őΉ����鑼�̃P�[�X�Ƃ̕������s�\��
�ł���Ƃ����������C�K�������g���₷���Ȃ���������܂���D
���̃����[�X�ł�

  tsapporo = temp.interpolate({"lon"=>141.0, "lat"=>43.0})

�Ƃł���悤�ɂ���\��ł����C����ł́C
((<interpo1_sapporo__.rb|URL:interpo1_sapporo__.rb>))
�̂悤�ɂ���K�v������܂��D

��������Ԑ�Ƃ��ė̈�O���w�肷��΁C((*���`�O�}*))�ɂȂ�܂��D�����ߖT�ȊO�ւ̊O�}�͈�ʂɍD�܂����Ȃ��̂Œ��ӂ��Ă��������D

�Ȃ��C(('interpolate')) �łȂ��C(('cut')) ���\�b�h���g���āC

  tnearS = temp.cut({"lon"=>141.0, "lat"=>43.0})

�Ƃ���ƁC141E, 43N �ɂ����Ƃ��߂��o�x�C�ܓx�̊i�q�_��I�Ԃ��ƂɂȂ�܂��D
���̏ꍇ�C���Z�͕K�v����܂���̂ŁCtnearS �̎��̂� temp 
�̃T�u�Z�b�g�ւ̃}�b�s���O�ƂȂ�܂��D
�i�ȏ�ɂ����� (('{ }')) �͏ȗ��ł��܂��D�j

=== �i�q�ɓ��}�^�΂߂ɐ؂�

��������Ԑ�̍��W�͕����_�ɂł��܂��D�Ⴆ�� lon, lat �� 5 �_���Ƃ�ꍇ�C
5�~5�̊i�q�_(�S25�_)�ɓ��}�������ꍇ�ƁC�ܓx�o�x�̔C�ӂ̑g�ݍ��킹�Ōv5�_�ɓ��}�������ꍇ������ł��傤�D(('interpolate')) �ł́C�O�҂�

  temp.interpolate(lon,lat)

��҂�

  temp.interpolate([lon,lat])

�Ƃ����`�Ŏ����ł��܂�(�����ŁC(('lon')), (('lat')) ��5�_�̊i�q�_��\�� 
((<VArray|URL:http://ruby.gfd-dennou.org/products/gphys/doc/varray.html>)))�D
�܂�C(({[lon,lat]})) �̂悤�ɔz��ɂ܂Ƃ߂Ĉ����Ƃ���ƁC
(({[ [lon[0],lat[0]], [lon[1],lat[1]],..]})) 
�Ƃ����`�̑g�ݍ��킹�œ��}��̊i�q�_���Ƃ�܂��D�ȉ��ɁC���ۂɓ��삷��T���v���v���O�����������܂��D

�T���v���v���O���� ((<interpo2_grid_slice.rb|URL:interpo2_grid_slice.rb>)):
  require "numru/ggraph"
  include NumRu

  #< open data / select a date for conciseness >

  temp = GPhys::IO.open("air.2010.nc","air")[false,3..8]  # 3..8 => Jan 4-9

  #< prepare new coordinates and interpolate >

  lon = VArray.new( NArray.float(5).indgen!+135, {"units"=>"degree_east"},"lon")
  lat = VArray.new( NArray.float(5).indgen!+34, {"units"=>"degree_north"},"lat")
  t_grid = temp.interpolate(lon,lat)
  t_slice = temp.interpolate([lon,lat])

  #< print >
  p "t_grid", t_grid
  p "t_slice", t_slice

  #< graphics >

  iws = (ARGV[0] || 1).to_i
  DCL.swpset('ldump',true) if iws==4
  DCL.swpset('iwidth',800)
  DCL.swpset('iheight',400)
  DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
  DCL.glpset('lmiss',true)
  DCL.gropn(iws)
  DCL.sldiv('y',2,1)
  GGraph.set_fig "itr"=>10
  GGraph.set_map "coast_japan"=>true
  GGraph.tone_and_contour t_grid
  GGraph.set_fig "itr"=>1
  GGraph.tone_and_contour t_slice.cut('level'=>850)
  DCL.grcls

���s���ʂ����Ɏ����܂��D�}���ƂQ�Ԗڂ̂͌o�x�ɂ��Ă���������Ȃ��̂�
�ܓx���ω����Ȃ�����}���Ă邱�Ƃ�������ɂ����ł��i���̓_�̉��P�� GGraph �̏����ۑ�j�C�W���o�͂�����ƁClon �Ƃ������� lat �Ƃ��������֌W�Â����Ă��邱�Ƃ��킩��܂�(AssocCoords�̗�)�D

((*���s����*))

�}((<(full size)|URL:interpo2_grid_slice.png>)) 
((<"IMG:interpo2_grid_slice_th.jpg">))

���s�ƕW���o��:
 % ruby interpo2_grid_slice.rb 4
 "t_grid"
 <GPhys grid=<4D grid <axis pos=<'lon' float[5] val=[135.0,136.0,137.0,138.0,...]>>
         <axis pos=<'lat' float[5] val=[34.0,35.0,36.0,37.0,...]>>
         <axis pos=<'level' shape=[17]  subset of a NumRu::VArrayNetCDF>>
         <axis pos=<'time' shape=[6]  subset of a NumRu::VArrayNetCDF>>>
    data=<'air' sfloat[5, 5, 17, 6] val=[282.029998779297,282.181213378906,282.332397460938,282.561614990234,...]>>
 "t_slice"
 <GPhys grid=<3D grid <axis pos=<'lon' float[5] val=[135.0,136.0,137.0,138.0,...]>>
         <axis pos=<'level' shape=[17]  subset of a NumRu::VArrayNetCDF>>
         <axis pos=<'time' shape=[6]  subset of a NumRu::VArrayNetCDF>>
         <AssocCoords  <'lat' float[5] val=[34.0,35.0,36.0,37.0,...]>
                 {["lon"]=>["lat"]}>>
    data=<'air' sfloat[5, 17, 6] val=[282.029998779297,280.350006103516,279.000396728516,277.755218505859,...]>>
  *** MESSAGE (SWDOPN) ***  GRPH1 : STARTED / IWS =  4.       
  *** WARNING (STSWTR) ***  WORKSTATION VIEWPORT WAS MODIFIED.  
  *** MESSAGE (SWPCLS) ***  GRPH1 : PAGE =   1 COMPLETED.      
  *** MESSAGE (SWDCLS) ***  GRPH1 : TERMINATED.  


== �}�E�X�N���b�N�Œf�ʐ؂�o��

((*����*)):
NCEP �ĉ�͂̋C���f�[�^ ((<air.2010.nc|URL:air.2010.nc>)) 
((-���̃f�[�^��((<NOAA�̃T�C�g|URL:http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html>))����擾���܂����D�{�t�@�C���ɂ�1-2���̃f�[�^�̂ݓ����Ă܂��D-)) 
��K���ȍ�ƃf�B���N�g���Ƀ_�E�����[�h���C������ cd ���܂��D�܂��Cirb�p���ʃX�^�[�g�A�b�v�t�@�C��
((<irbrc_ggraph.rb|URL:../irbrc_ggraph.rb>)) ���z�[���f�B���N�g�� "(('~'))" 
�܂��͌��݂̃f�B���N�g���Ƀ_�E�����[�h���܂��D�ȉ��ł̓z�[���f�B���N�g���ɒu�������̂Ƃ��Ęb��i�߂܂��D


����܂ł̗�ł́C��Ԑ�̍��W�͂��炩���߃v���O�������ŗ^���܂����D
(({interpolate})) �̉��p���\�b�h�ł��� (({mouse_cut})) ���g���ƁC
�}�E�X�Ŏw�肷�邱�Ƃ��ł��܂��D2�����Ő}�����đΘb�I�ɒf�ʂ�؂�o���̂ł��D


�����ł́C���s����ŌJ��Ԃ��̂ɓK���� irb �Ŏ��s���Ă݂܂��傤(��Ɏ��s�v���O�������f�ڂ��܂�)�D�܂��C�R�}���h���C���Ɏ�����͂��܂� (irbrc_ggraph.rb�t�@�C�����J�����g�f�B���N�Ƃɂ������ꍇ�́C"~/" �̕����͂Ƃ��ē���)�D

  irb -r ~/irbrc_ggraph.rb

����ƁCirb �̓��̓v�����v�g�������̂ŁC�ȉ����R�s�[���y�[�X�g�ŗ������݂܂��D

  temp = open("air.2010.nc/air").cut("time"=>Date.parse("2010-01-10"))
  set_map "coast_world"=>true
  set_fig "itr"=>30
  tone temp
  tcut,line = temp.mouse_cut(0,1)
  next_fig "itr"=>2 ; tone tcut; color_bar

5�s�ڂ́C(({temp.mouse_cut(0,1)})) �����s����ƁC


   '*** Waiting for mouse click. Click 2 points in the current viewport.

�Ƃ����\�����łāC�}�E�X�N���b�N��҂��܂��̂ŁC�`�悳��Ă�͈͓���2�_�N���b�N���Ă��������D
�͈͊O���N���b�N����ƁC���Ȃ��������߂��܂��D

(('mouse_cut')) �̈��� (('(0,1)')) �Ŏw�肵�Ă���̂́C�}��x,y���ɑ������鎟���� 
(('temp')) �ɂ����Ă͉��Ԗڂ̎������Ƃ������Ƃł�(0���琔���܂��D(('"lon"')),
(('"lat"')) �̂悤�ɖ��O�ł��w��ł��܂�)�D
�N���b�N����}�� (('mouse_cut')) �Ăяo�������O�ɏ��������̂Ȃ̂ŁC�}�̎����؂�o���Ώۂ̂ǂ̎��ɓ����邩���w�肷��K�v������̂ł��D

(('mouse_cut')) �̖߂�l��2����C�ŏ��� ((('tcut'))) �͐؂�o���œ���ꂽ 
GPhys �I�u�W�F�N�g�C2�Ԃ߂� ((('line'))) �́C�؂�o���ɂ���������
(DCLMouseLine�N���X�̃I�u�W�F�N�g) �ł��D
�Ȃ��C(('mouse_cut')) ���ĂԑO�̐}�ł� ((*�J���[�o�[��\�����Ȃ��ł�������*))�D
�\������ƁCDCL �̃r���[�|�[�g�̎�蒼�����������邽�߁C�������f�ʂ��Ƃ�܂���D
(���̖��͏����̔łŉ������邩������܂���D)

���āC2�_���N���b�N����ƁC���̊Ԃ����Ԑ������\������(���̐}a: �킩��ɂ����ł����C���{����k�ɂ�ʂ��Ĕ��Α��ɐL�т��)�C���̐��ɉ����Ēf�ʂ�؂�o���C���̍s�Ő}������܂�(�}b)�D
���̊Ԃɉ��_�Ƃ邩��2�_�Ԃ̊i�q�_���ɉ����Ă��悻�̕���\��ۑ�����悤���߂��܂��D
�Ȃ��C�}�̂悤�ɁC�n�}���e���Ă���ꍇ���C�}�̒����ɂ����Đ؂�o����܂��D

�}a((<(full size)|URL:interpo3_mouse_1.png>))
((<"IMG:interpo3_mouse_1_th.jpg">))
�}b((<(full size)|URL:interpo3_mouse_2.png>)) 
((<"IMG:interpo3_mouse_2_th.jpg">))

�n�}���e�̏ꍇ�C����W���͑�~��̋����i�P�ʂ͓x�j�ɂȂ�܂��D
���ꂪ�ŗǂƂ͌���܂��񂪁C���e���ꂽ������̋����ɂ���ƒP�ʂ��s���m�ł��̂ŁD
�؂�o������ (('tcut')) �� (('p')) �R�}���h�ŕ\������ƁC�ܓx�o�x���⏕���W�Ƃ��ē����Ă��邱�Ƃ��킩��܂��F

  irb(main)> p tcut
  <GPhys grid=<2D grid <axis pos=<'dist' float[69] val=[0.0,0.28327150864204,0.566310823555379,0.849106337409739,...]>>
          <axis pos=<'level' shape=[17]  subset of a NumRu::VArrayNetCDF>>
          <AssocCoords  <'lon' float[69] val=[20.2975883483887,20.4134693145752,20.5362205505371,20.6664447784424,...]>
                  <'lat' float[69] val=[79.9568328857422,80.2394027709961,80.5216979980469,80.8037033081055,...]>
                  {["dist"]=>["lon", "lat"]}>>
     data=<'air' sfloat[69, 17] val=[250.371932983398,248.930969238281,247.500518798828,246.070541381836,...]>>

�`�悪�n�}���e�łȂ��ꍇ�C2�̍��W���̂����C�N���b�N�łƂ��������L����������W�ϐ��C����������⏕���W�ϐ��ƂȂ�܂��D

�ȏ�Cirb ���g����������܂������C�������Ƃ��v���O���� ((<interpo3_mouse.rb|URL:interpo3_mouse.rb>))
�ł����s�ł��܂��D

���āC((*��x�}�E�X�ōs�����؂�o���́C�J��ւ����čs�����Ƃ��ł��܂�*))�D���Ƃ��΁C(("uwnd")) 
�Ƃ����ϐ��ɕ����������Ă����Ƃ���ƁC

  ucut,line = unwd.mouse_cut_repeat

�Œ��O�̐؂�o�����J��Ԃ��܂��i���O�Ɏg������Ԑ�̍��W�ϐ����ۑ�����Ă���̂ł�����g���j�D


�؂�o����2�_�Ԃ����łȂ�((*3�_�ȏ�̐܂���ɂ����Ă��ł��܂�*))�F

  tone temp
  tcut,line = temp.mouse_cut(0,1,3)
  next_fig "itr"=>2 ; tone tcut; color_bar

(('mouse_cut')) �̑�3�����ł́C(�܂�)���̓_�����w�肵�܂��D
�ȗ��l��2�ƂȂ��Ă��܂��̂ŁC�f�t�H���g��2�_�Ԃ̐�����ł̐؂�o���Ȃ̂ł��D
���̐}�́C���A�W�A��3�_�Ƃ�����������܂��D

�}((<(full size)|URL:interpo3_mouse_3.png>))
((<"IMG:interpo3_mouse_3_th.jpg">))
�}((<(full size)|URL:interpo3_mouse_4.png>)) 
((<"IMG:interpo3_mouse_4_th.jpg">))



== ���W�ϊ�

((*����*)): 
NCEP �ĉ�͂̋C�� ((<air.2010.nc|URL:air.2010.nc>)) ����ѓ�����
((<uwnd.2010.nc|URL:uwnd.2010.nc>)) �̓����ϒl�f�[�^
((-���̃f�[�^��((<NOAA�̃T�C�g|URL:http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html>))����擾���܂����D�{�t�@�C���ɂ�1-2���̃f�[�^�̂ݓ����Ă܂��D-)) 
��K���ȍ�ƃf�B���N�g���Ƀ_�E�����[�h���C������ cd ���܂��D

=== �C�ӂ�1�����̕ϊ��i�������W�ϊ��C���ʍ��W�̓����Ȃǁj

��C�E�C�m�̐��l���f�����O�ł͒ʏ�C���ʂ̂���n�\�ʂ����l�ƂȂ�悤�ȉ������W�i�Ѝ��W�Ȃǁj���p�����܂��D������C���x��C���Ɋ�Â������W�ɕϊ�����̂͊�{�I�ȍ�Ƃł��D�܂��C���ʂ̂悤�ɔ�r�I�P�����E�ۑ����̍��������ʂ����W���ɂƂ邱�Ƃ��s���܂��D���̂悤�ȕϊ��ł́C�ϊ��Ώۂ�1�����i�ȏ�̗�ł͉������W�j�ł����C�ϊ��O��̍��W�̑Ή��͑��̋�Ԏ��⎞�Ԏ��̊֐��ƂȂ�܂��D�����ł͂��̂悤�ȕϊ��������܂��D

GPhys �I�u�W�F�N�g�́C�z��̊e���ɑΉ�����1������(��)���W�ϐ��ɉ����C1�`�����̎��ɑΉ��ł���1�`�������́u�⏕���W�v�������Ƃ��ł��܂��D(('interpolate')) ���\�b�h�̈����ɂł���̂́C����W�ɂȂ肤��1������ VArray �݂̂ł����C���ꂼ��� VArray �͑������̕⏕���W�ɑΉ��ł��܂��i�Ή��͖��O�Ō��܂�܂��̂ŁC����⏕���W�ɑΉ�����1���� VArray �Ƃ́C���̕⏕���W�Ɠ����������O������ VArray �ł��j�D�W�L�̍��W�ϊ��́C�⏕���W���g���Ď������܂��D

�����ł́C���ʍ��W�ɕϊ�����T���v���v���O�������g���Đ������܂��D

�T���v���v���O���� ((<interpo4_theta_coord.rb|URL:interpo4_theta_coord.rb>)):
  require "numru/ggraph"
  include NumRu

  #< interpret command-line arguments > 

  iws = (ARGV[0] || 1).to_i

  #< open data > 

  temp = GPhys::IO.open("air.2010.nc","air")[false,2..-1,{0..20,10}]
  uwnd = GPhys::IO.open("uwnd.2010.nc","uwnd")[false,2..-1,{0..20,10}]

  #< calculate potential temperature (theta) > 

  prs = temp.axis("level").to_gphys
  p00 =  UNumeric[1000.0, "millibar"]
  kappa = 2.0 / 7.0
  pfact = (prs/p00)**(-kappa)
  theta = temp * pfact
  theta.name = "theta"
  theta.long_name = "potential temperature"

  #< set theta as an associated coordinate >

  uwnd.set_assoc_coords([theta])
  p "uwnd:", uwnd

  #< prepare a theta coordinate variable >

  tht_crd = VArray.new( NArray[300.0,350.0, 400.0, 500.0, 700.0, 800.0], 
                        {"units"=>"K"}, "theta")

  #< transform the vertical coordinate to theta >

  uwnd_ontht = uwnd.interpolate("level"=>tht_crd)

  #< graphics >

  DCL.swpset('iwidth',800)
  DCL.swpset('iheight',400)
  DCL.swpset('ldump',true) if iws==4
  DCL.gropn(iws)
  DCL.sldiv('y',2,1)
  DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
  DCL.glpset('lmiss',true)

  GGraph::set_fig "itr"=>2,"viewport"=>[0.16,0.73,0.2,0.8]
  GGraph::tone_and_contour uwnd.mean(0),true
  GGraph::color_bar

  GGraph::set_fig "itr"=>1
  GGraph::tone_and_contour uwnd_ontht.mean(0),true
  GGraph::color_bar

  DCL.grcls

���s
  ruby interpo4_theta_coord.rb

���s����((<(full size)|URL:interpo4_theta_coord.png>))
((<"IMG:interpo4_theta_coord_th.jpg">))

�v���O�����͏��X�����ł����C�����͏�����`�敔���ł���C���W�ϊ����̂��̂Ɋւ��镔���͒Z���Ȃ��Ă��܂��D
�t�@�C�����J�������ƁC((|�� = T*(p00/p)^��|)) (p�͋C���Cp00 �͒萔=1000hPa,
�� = 2/7) �ɂ���āC���ʃƂ����߂܂��D�Ƃ͉����ɂقڒP������ƂȂ�܂��D

���ŁC

  uwnd.set_assoc_coords([theta])

�ɂ���āC(('theta')) �� (('uwnd')) �̕⏕���W�Ƃ��܂��D
(('set_assoc_coords')) �̈����́C�⏕���W�ɂ��� GPhys �I�u�W�F�N�g����ׂ��z��ɂȂ�܂�
((-�⏕���W�ƂȂ� GPhys �I�u�W�F�N�g�ɂ��������W�́C�⏕���W��^������ GPhys �I�u�W�F�N�g�̎���W�̒��Ɋ܂܂�ĂȂ��ƂȂ�܂��񂪁C(('theta')) �� (('uwnd')) �͓������W�Œ�`����Ă���̂ŁC���̏����𖞂����܂�-))�D
���̎��_�� (('uwnd')) �� (('p')) �ŕ\������ƁC���̂悤�ɂȂ�܂��D

  "uwnd:"
  <GPhys grid=<4D grid <axis pos=<'lon' shape=[144]  subset of a NumRu::VArrayNetCDF>>
          <axis pos=<'lat' shape=[73]  subset of a NumRu::VArrayNetCDF>>
          <axis pos=<'level' shape=[15]  subset of a NumRu::VArrayNetCDF>>
          <axis pos=<'time' shape=[3]  subset of a NumRu::VArrayNetCDF>>
          <AssocCoords  <'theta' float[144, 73, 15, 3] val=[273.1746199385,273.1746199385,273.1746199385,273.1746199385,...]>
                  {["lon", "lat", "level", "time"]=>["theta"]}>>
     data=<'uwnd' shape=[144, 73, 15, 3]  subset of a NumRu::VArrayNetCDF>>

�⏕���W�����w�肵�Ă��܂��΁C���Ƃ́C�ʏ�̕�ԂƓ��l�ł��D
�܂�C��Ԑ�ƂȂ�ꎟ���̍��W��p�ӂ��C(('interpolate')) ���ĂԂ����ł��D
�������C���̏ꍇ�C(('theta')) ���Ή����鎟���� lon, lat, level, time 
��4�Ȃ̂ŁC���̂����ǂ̍��W��ϊ����邩�͗z�Ɏw�肷��K�v������܂��D
���̂��߁C���̂悤�ȋL�@�Ŏw�肵�܂��F

  uwnd_ontht = uwnd.interpolate("level"=>tht_crd)

���Ȃ݂� ((|��|)) �������ɒP������łȂ��ꍇ�C��Ԑ�ƂȂ肤��ꏊ����������ꍇ������܂��D
���̏ꍇ�ɂ�����Ɍ��܂邩�� GPhys 1.0.0 �ł͎��̂悤�ɂȂ�܂�(�����̔łł͕ς��\��������܂�)�D�ŏ��̒T���́C���W�ϐ��̔z��̓Y�������������ɍs���܂��̂ŁC���W���̊i�[���Ō��܂�܂��D
���� (('interpolate')) �R�[�����ɂ�����2��ڂ߈ȍ~�̒T���ł́C���̂悤�ȕۏ؂͂���܂��񂪁C�������̂��߁C�A�������T���͑O��̒T�����ʂ̓Y���̈ʒu����͂��߂܂��̂ŁC������x�A���I�ł��邱�Ƃ����҂���܂��i��Ԃ��鎲�ȊO�̎�����������Ƃ��܂肻���Ȃ�܂��񂪁j�D

((*�⑫*))�F���l���f���f�[�^�̏����ŕp�o����C�n�`�ɉ��������W�̕ϊ����C��Ɠ��l�ɕϊ���̗ʁi���x��C���j��⏕���W�Ƃ��� interpolate ��K�p����΂ł��܂��D

=== 2�������W�ϊ�

�����ň����̂́C�ɍ��W�ł̊i�q�_���f�J���g���W�ł̊i�q�_�ɂ���Ƃ��C�K���ȓ��e�@�ɂ��Ƃ����n�}��ł̊i�q�_���ܓx�o�x���W�ɂ���Ƃ������C1�����ł̕�Ԃɂ͊Ҍ������Ȃ����W�ϊ��ł��D

�n�}��̍��W�ɂ�����i�q�_�f�[�^���ܓx�o�x���W�ŕ�Ԃ������Ƃ肠���܂��D�n�}��̍��W�� x, y, �ܓx�o�x���W��
lon, lat �Ƃ��܂��D����ɕϊ��ΏۂƂȂ� GPhys �ϐ��͍��x z �⎞�� t 
�̊֐��ł������������ł��傤�D�ϊ��O�� GPhys �I�u�W�F�N�g gp 
���Cx, y, z, t �̊e���ŋK���I�Ɂi�u�����`�v�I�Ɂj�T���v������Ă��Ƃ���ƁC
lon, lat �Ɋւ���⏕���W(�Ƃ��� x, y �����W�Ƃ���2���� GPhys �I�u�W�F�N�g)��ݒ肵����ŁC

  gplonlat = gp.interpolate(vlon,vlat)

�̂悤�ɐ؂�o�����s���܂��D�����ŁC(('vlon')), (('vlat')) 
�́C1������ VArray �ł��D gplonlat �͌o�x�C�ܓx�C���x�C������4������GPhys
�ɂȂ�܂��D
�O�q�̉������W�ϊ��̏ꍇ�ƈႢ�C�ǂ̍��W�Ɋւ��ĕ�Ԃ��s�����̔C�Ӑ��͂Ȃ��̂ŁC���W�ϐ������w�肷��K�v�͂���܂���D����ŁC�o�x�ܓx���W�ɂ����āu�����`�I�v�Ȕz�u�̊i�q�֕�Ԃ���܂��D����C

  gplonlat = gp.interpolate([vlon,vlat])

�̂悤�ɁC�o�x�C�ܓx�i�q�_�l��z��ɂ܂Ƃ߂ēn���ƁC(('[ [vlon[0],vlat[0]], [vlon[1],vlat[1]],..]'))
�̓_��ɕ�Ԃ��s���܂��i������񂱂̏ꍇ�� 
(('vlon')) �� (('vlat')) �̒����͈�v���Ȃ���΂Ȃ�܂���j�D��̓I�ȗ��p���
((<�\�[�X�t���̗��p��>)) �̐߂ŏЉ��CGPhys 
���C�u�����t���̃e�X�g�v���O�����ɂ���̂ŁC�Q�Ƃ��Ă��������D

�O�߂ŏq�ׂ�����1���������\�ȍ��W�ϊ����ƈႢ�C�{�߂ŏq�ׂ��悤�ȕ�Ԃ́C�������ł̑Ή��i�q�_�T�����K�v�ł��D
���̏ꍇ�́C���݂�((*2�����܂ł����T�|�[�g���Ă��炸�C�߂������Ɋg���̗\�������܂���*))�D�܂��C���̃P�[�X�ł͓��}�݂̂ŊO�}�͍s���܂���D�̈�O���w�肷��ƁC�������� DCL �� gt2dlib �̎d�l�ɂ���O���������܂��D

== �i�q���킹

(({interpolate})) �̉��p���\�b�h���āC��� GPhys �̊i�q�����킹��
(({regrid})) ������܂��D����́C

  gphys_re = gphys_from.regrid(gphys_to)

�Ƃ����`�ł����܂��D(({gphys_to})) �̊i�q�_�� (({gphys_from}))
���T���v�����O�������̂� (({gphys_re})) �Ƃ��ĕԂ��܂��D

�قȂ�i�q�_�Œ�`���ꂽ�f�[�^�Ԃŉ��Z�������΂����C���Z�O�� (({regrid}))
�ň���𑼕��̊i�q�ɍ��킹�Ă��������D

�Ȃ��C(({regrid})) �́C�\�[�X�����ꂾ���̊ȒP�ȃ��\�b�h�ł��F

    def regrid(to)
      coords = to.axnames.collect{|nm| to.coord(nm)}
      interpolate(*coords)
    end

== ����ɂ��킵��

=== �\�[�X�t���̗��p��

����܂ŏq�ׂĂ�����ԂɊւ��郁�\�b�h (({interpolate})), (({mouse_cut})), 
(({mouse_cut_repeat})), (({regrid})) �̊e���\�b�h�́C
(('interpolate.rb')) �Ƃ����t�@�C���ɒ�`����Ă��܂�
�i�\�[�X�̃g�b�v�f�B���N�g���ȉ��܂��̓C���X�g�[����ł̃p�X�� (('lib/numru/gphys/interpolate.rb'))�ł��j�D

(('interpolate.rb')) �����ɂ́C�e�X�g�v���O�����Ƃ����`�ŗl�X�ȗ��p�Ⴊ����܂��D
�����ł̓f�[�^��ǂݍ��܂��C������ GPhys �I�u�W�F�N�g���ꂩ�琶�����ėp���܂��̂ŁC
�d�l�̊m�F�ɂ͂悢�ł��傤�D

�܂��CGPhys �\�[�X�̃g�b�v�f�B���N�g�������� (('sample')) �Ƃ����f�B���N�g���ɂ́C
(('ncep_theta_coord.rb')) �Ƃ����T���v���v���O����������܂��D
����́C��ŏЉ�����ʍ��W�ϊ��ł����C�f�[�^�ɂ� OPeNDAP 
�Ƃ������u�ʐM�ŃA�N�Z�X������C�R�}���h���C�������������Ȃǂ̓���������܂��D

GPhys 1.0.0 �ɂ����� (('interpolate.rb')) �̃e�X�g�������f�ڂ��܂��F

  require "numru/ggraph"
  include NumRu
  include NMath

  module NumRu
    class VArray
      def to_g1D
        ax = Axis.new().set_pos(self)
        grid = Grid.new(ax)
        GPhys.new(grid,self)
      end
    end
  end

  #< prepare a GPhys object with associated coordinates >

  nx = 10
  ny = 8
  nz = 2
  x = (NArray.sfloat(nx).indgen! + 0.5) * (2*PI/nx)
  y = NArray.sfloat(ny).indgen! * (2*PI/(ny-1))

  z = NArray.sfloat(nz).indgen! 
  vx = VArray.new( x, {"units"=>"m"}, "x")
  vy = VArray.new( y, {"units"=>"m"}, "y")
  vz = VArray.new( z, {"units"=>"m"}, "z")
  xax = Axis.new().set_pos(vx)
  yax = Axis.new().set_pos(vy)
  zax = Axis.new().set_pos(vz)
  xygrid = Grid.new(xax, yax)
  xyzgrid = Grid.new(xax, yax, zax)

  sqrt2 = sqrt(2.0)

  p = NArray.sfloat(nx,ny)
  q = NArray.sfloat(nx,ny)
  for j in 0...ny
    p[true,j] = NArray.sfloat(nx).indgen!(2*j,1)*sqrt2
    q[true,j] = NArray.sfloat(nx).indgen!(2*j,-1)*sqrt2
  end
  vp = VArray.new( p, {"units"=>"mm"}, "p")
  vq = VArray.new( q, {"units"=>"mm"}, "q")
  gp = GPhys.new(xygrid, vp) 
  gq = GPhys.new(xygrid, vq) 

  r = NArray.sfloat(nz).indgen! * 2
  vr = VArray.new( r ).rename("r")
  gr = GPhys.new( Grid.new(zax), vr ) 

  d = sin(x.newdim(1,1)) * cos(y.newdim(0,1)) + z.newdim(0,0)
  vd = VArray.new( d ).rename("d")
  gd = GPhys.new(xyzgrid, vd)

  gx = vx.to_g1D
  ga = gd + gx 
  ga.name = "a"

  gd.set_assoc_coords([gp,gq,gr,ga])

  print "GPhys with associated coordinates:\n"
  p gd

  DCL.swpset('iwidth',700)
  DCL.swpset('iheight',700)
  DCL.gropn(1)
  DCL.glpset("lmiss",true)
  DCL.sldiv("y",2,2)
  GGraph::set_fig "viewport"=>[0.15,0.85,0.15,0.85]
  GGraph::tone gd
  GGraph::color_bar
  GGraph::tone gd[true,ny/2,true]
  GGraph::color_bar

  #< prepare coordinates to interpolate >

  xi = NArray[1.0, 2.0, 3.0, 4.0, 5.0]
  yi = NArray[-0.1, 2.5, 4.0, 5.5, 6.8]  # test of extrapolation
  vxi = VArray.new( xi, {"units"=>"m"}, "x")  # "0.5m" to test unit conversion
  vyi = VArray.new( yi, {"units"=>"m"}, "y")  # "0.5m" to test unit conversion

  pi = NArray.float(6).indgen!*2+10
  qi = NArray.float(6).indgen!*2
  vpi = VArray.new( pi, {"units"=>"mm"}, "p")
  vqi = VArray.new( qi, {"units"=>"mm"}, "q")

  ai = NArray[2.0, 4.0]
  vai = VArray.new( ai ).rename("a")

  #< test of interpolate >

  gxi = vxi.to_g1D
  gyi = vyi.to_g1D
  gp = GPhys.new(xygrid,vp)
  gq = GPhys.new(xygrid,vq)

  gi = gd.interpolate(vxi,vyi,{"z"=>0.5})
  GGraph::tone gi,true,"color_bar"=>true

  ###gd.interpolate(vxi,vyi,vr,vz)   # nust fail by over-determination

  gi = gd.interpolate([vxi,vyi])
  GGraph::tone gd,true,"min"=>-1.2,"max"=>1.2,"int"=>0.1
  GGraph::scatter gxi, gyi, false,"type"=>4,"size"=>0.027,"index"=>3
  GGraph::color_scatter gxi, gyi, gi, false,"min"=>-1.2,"max"=>1.2,"int"=>0.1,"type"=>10,"size"=>0.029
  GGraph::color_bar

  gi = gd.interpolate(vyi,vxi)
  GGraph::tone gi,true,"color_bar"=>true

  #GGraph::tone gp,true,"color_bar"=>true

  GGraph::tone gq,true
  GGraph::contour gq,false
  GGraph::color_bar

  gi = gd.interpolate(vxi,vqi)
  GGraph::tone gi,true,"color_bar"=>true

  gi = gd.interpolate("y"=>vqi)

  gi = gd.interpolate("y"=>vai)
  GGraph::tone gi[2,false],true,"color_bar"=>true

  GGraph::tone gp,true
  GGraph::contour gp,false
  GGraph::color_bar
  gi = gd.interpolate("x"=>vpi)
  GGraph::tone gd
  GGraph::tone gi,true,"color_bar"=>true,"exchange"=>true,"min"=>-1,"max"=>1

  gi = gd.interpolate([vpi,vqi])
  GGraph::tone gi,true,"color_bar"=>true

  GGraph::tone gd
  GGraph::tone gd.cut("p"=>vpi.min.to_f..vpi.max.to_f,"q"=>vqi.min.to_f..vqi.max.to_f),true

  gi = gd.interpolate(vpi,vqi)
  GGraph::tone gi,true,"color_bar"=>true

  gi = gd.interpolate(vqi,vpi)
  GGraph::tone gi,true,"color_bar"=>true

  gi2 = gd.regrid(gi[false,0])
  p "regriding test (should be true):", gi.val == gi2.val

  gi = gd.interpolate(vqi,vpi,{"z"=>0.5})
  GGraph::tone gi,true,"color_bar"=>true

  mask=d.lt(0.7)
  missv = -999.0
  d[mask.not] = missv
  p d[false,0]
  dm = NArrayMiss.to_nam(d, mask )
  vdm = VArray.new( dm, {"missing_value"=>NArray[missv]}, "d")
  gdm = GPhys.new(xyzgrid, vdm)
  gi = gdm.interpolate(vpi,vqi)
  GGraph::tone gi,true,"color_bar"=>true

  #< finish >
  DCL.grcls

���s����

((<(full size)|URL:interpolate_rb_testpart1.png>))
((<"IMG:interpolate_rb_testpart1_th.jpg">))
((<(full size)|URL:interpolate_rb_testpart2.png>))
((<"IMG:interpolate_rb_testpart2_th.jpg">))

((<(full size)|URL:interpolate_rb_testpart3.png>))
((<"IMG:interpolate_rb_testpart3_th.jpg">))
((<(full size)|URL:interpolate_rb_testpart4.png>))
((<"IMG:interpolate_rb_testpart4_th.jpg">))

((<(full size)|URL:interpolate_rb_testpart5.png>))
((<"IMG:interpolate_rb_testpart5_th.jpg">))


=== ��Ԃ̎d�l

�ȉ��Ƀ}�j���A�����ڂ��܂��D

---interpolate(*coords)

   Wide-purpose multi-dimensional linear interpolation
   
   This method supports interpolation regarding combinations of 
   1D and 2D coordinate variables. For instance, suppose self is
   4D with coordinates named ["x", "y", "z", "t"] and associated
   coordinates "sigma"["z"] ("sigma" is 1D and its axis is "z"),
   "p"["x","y"], "q"["x","y"] ("p" and "q" are 2D having the
   coordinates "x" and "y"). You can make interpolation by
   specifying 1D VArrays whose names are among "x", "y", "z", "t",
   "sigma", "p", "q". You can also use a Hash like {"z" => 1.0}
   to specify a single point along the "x" coordinate.
   
   If the units of the target coordinate and the current coordinate
   are different, a converstion was made so that slicing is
   made correctly, as long as the two units are comvertible;
   if the units are not convertible, it is just warned.
   
   If you specify only "x", "y", and "t" coordinates
   for interpolation, the remaining coordinates "z" is simply
   retained. So the result will be 4 dimensional 
   with coordinates named ["x", "y", "z", "t"], but the
   lengths of "x", "y", and "t" dimensions are changed according
   to the specification. Note that the result could 
   be 3-or-smaller dimensional -- see below.
   
   Suppose you have two 1D VArrays, xnew and ynew, having
   names "x" and "y", respectively, and the lengths of xnew and
   the ynew are the same. Then, you can give an array of 
   the two, [xnew, ynew], for coord0 as
   
     gp_int = gp_org.interpolate( [xnew, ynew] )
   
   (Here, gp_org represents a GPhys object, and the return value
   pointed by gp_int is also a GPhys.)  In this case, 
   the 1st dimension of the result (gp_int) will be sampled
   at the points [xnew[0],ynew[0]], [xnew[1],ynew[1]], [xnew[2],ynew[2]], 
   ..., while the 2nd and the third dimensions are "z" and "t" (no 
   interpolation). This way, the rank of the result will be reduced 
   from that of self.
   
   If you instead give xnew to coord0 and ynew to coord1 as 
   
     gp_int = gp_org.interpolate( xnew, ynew )
   
   The result will be 4-dimensional with the first coordinate
   sampled at xnew[0], xnew[1], xnew[2],... and the second
   coordinate sampled at ynew[0], ynew[1], ynew[2],...
   You can also cut regarding 2D coordinate variable as
   
     gp_int = gp_org.interpolate( pnew, qnew )
     gp_int = gp_org.interpolate( xnew, qnew )
     gp_int = gp_org.interpolate( [pnew, qnew] )
     gp_int = gp_org.interpolate( [xnew, qnew] )
   
   In any case, the desitination VArrays such as xnew ynew pnew qnew
   must be one-dimensional.
   
   Note that
   
     gp_int = gp_org.interpolate( qnew )
   
   fails (exception raised), since it is ambiguous. If you tempted to
   do so, perhaps what you want is covered by the following special
   form:
   
   As a special form, you can specify a particular dimension
   like this:
   
     gp_int = gp_org.interpolate( "x"=>pnew )
   
   Here, interpolation along "x" is made, while other axes are
   retained. This is useful if pnew corresponds to a multi-D
   coordinate variable where there are two or more corresponding axes
   (otherwise, this special form is not needed.)
   
   See the test part at the end of this file for more examples.
   
   LIMITATION
   
   Currently associated coordinates expressed by 3D or greater
   dimensional arrays are not supported.
   
   Computational efficiency of pure two-dimensional coordinate
   support should be improved by letting C extensions cover deeper
   and improving the search algorithm for grid (which is usually 
   ordered quasi-regularly).
   
   COVERAGE
   
   Extrapolation is covered for 1D coordinates, but only
   interpolation is covered for 2D coordinates (which is
   limited by gt2dlib in DCL -- exception will be raised
   if you specify a grid point outside the original 2D grid points.).
   
   MATHEMATICAL SPECIFICATION
   
   The multi-dimensional linear interpolation is done by
   supposing a (hyper-) "rectangular" grid, where each 
   dimension is independently sampled one-dimensionally. In case
   of interpolation along two dimensional coordinates such as "p" 
   and "q" in the example above, a mapping from a rectangular grid
   is assumed, and the corresponding points in the rectangular grid 
   is solved inversely (currently by using gt2dlib in DCL).
   
   For 1D and 2D cases, linear interpolations may be expressed as
   
      1D:  zi = (1-a)*z0 + a*z1
      2D:  zi = (1-a)*(1-b)*z00 + a*(1-b)*z10 + (1-a)*b*z01 + a*b*z11 
   
   This method is extended to arbitrary number of dimensions. Thus, 
   if the number of dimensions to interpolate is S, then 2**S grid
   points are used for each interpolation (8 points for 3D, 16 points
   for 4D,...).  Thus, the linearity of this interpolation is only along 
   each dimension, not over the whole dimensionality.
   
   USAGE
     interpolate(coord0, coord1, ...)

   ARGUMENTS
   * coord0, coord1,... [ 1D VArray, or Array of 1D VArray,
     or a 1-element Hash as 
     {coordinate_name(String) => slice_loc_value(Numeric)} ] :
     locations to which interpolation is made. Names of 
     all the VArray's in the arguments must exist among
     the names of the coordinates of self (including associated
     coordinates), since the dimension
     finding is made in terms of coordinate names.
     If an argument is an Array of VArray's, the first
     VArray will become the main coordinate variable,
     and the rest will be associated coordinates.
   * [SPECIAL CASE]
     You can specfify a one-element Hash as the only argument
     such as
          gphys.interpolate("x"=>varray)
     where varray is a coordinate onto which interpolation is made.
     This is espcially useful if varray is multi-D. If varray's 
     name "p" (name of a 2D coordnate var), for example, 
     you can interpolate only regarding "x" by retaining other
     axes. If varray is 1-diemnsional, the same thing can
     be done simply by 
          gphys.interpolate(varray)
     since the corresponding 1D coordinate is found aotomatically.
   
   RETURN VALUE
   * a GPhys
 

---mouse_cut(dimx, dimy, num=2)
   Makes a subset interactively by specifying a (poly-)line on the DCL viewport
   
   ARGUMENTS
   * dimx {String] : name of number (0,1,..) of the dimension
     corresponding to the X coordinate in the current window of DCL
   * dimy {String] : name of number (0,1,..) of the dimension
     corresponding to the Y coordinate in the current window of DCL
   * num {Integer] : the number of points along the (poly-)line
     (2 or greater -- if 2, a single line segment; if 3 or more, a 
     poly-line)
   
   RETURN VALUE
   * a GPhys

---mouse_cut_repeat
   Interpolation onto grid points specified by the previous call of GPhys#mouse_cut

---regrid(to)
   Interpolate to conform the grid to a target GPhys object
    
   ARGUMENTS
   * to [GPhys] : the target gphys
   
   RETURN VALUE
   * a GPhys
   
