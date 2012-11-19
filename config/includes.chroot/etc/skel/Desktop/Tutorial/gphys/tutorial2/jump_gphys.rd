# GPhys, GGraph �`���[�g���A�� (�W�����v. GPhys ��)
#
# = ���� (�V�������̂͏ォ��ǋL)
#
# * 2012/02/18 �[���N�j  �V�K�쐬
#

((*���̃h�L�������g�͍쐬���ł�. �w�K�ɂ�
((<"���s�̃`���[�g���A��"|URL:http://ruby.gfd-dennou.org/products/gphys/tutorial/>))
���Q�Ƃ�������.*))

== GPhys ���g���Ă�����Ɖ��p

GPhys ���C�u������{�i�I�Ɏg���Ă݂܂��傤.

=== GPhys �I�u�W�F�N�g�̑���

�ēx T.nc ��ǂݍ��݂܂�.

  gp = gpopen('T.nc/T')

����܂ł͑S���E��`���Ă��܂�����, ���{�t�߂�؂�o���Ă݂܂��傤.
�ǂ̏ꏊ�����Ă��邩������₷���悤�A�C�ݐ���`���悤�ɂ��Ă����܂�.

  set_fig('itr' => 10 )  # �����~���}�@
  set_map('coast_world'=>true)   # �n���̊C�ݐ���\��

�؂�o�����߂ɂ�, �؂�o�������w�肷��K�v������܂�.
gp �̒��Ɋi�[����Ă��鎲�����Ă݂܂��傤.

  gp

"axis pos=" �̌��ɂ���̂����̖��O�ł�.
# �f�[�^�Ǝ����Z�b�g�ɂȂ��Ă��邱�Ƃ� GPhys �̔���Ƃ��ċ������ׂ�.

lat, lon �����ꂼ��ܓx�o�x�Ɛ�������܂��̂�, 
����Ő؂��Ă݂܂��傤.

  tone(gp.cut('lat'=>20..50, 'lon'=>120..150))  # �ܓx 20-50 �x, �o�x 120-150 �x�͈̔͂�؂�o���ĕ`��

���̂Ƃ�, �w�肵���ܓx�o�x�Ƀf�[�^�����݂��Ȃ��Ă�, 
GPhys ���ŋ߂��l��T���Ď���Ă��Ă���܂�.

���Ȃ݂ɂ����, 

  gp_jpn = gp.cut('lat'=>20..50, 'lon'=>120..150)
  tone(gp_jpn)

�ƕ����ď������Ƃ��ł��܂�.
���Ƃ��Γ��{�t�߂̐}���������`�������Ƃ��Ȃǂɓ��͂����点��̂Ŋy�ł�.

cut �ɒl���ЂƂ��������Ƃ��̖ʂł̐؂�o���ɂȂ�܂�.
���߂��ɓ��{���̉����v���t�@�C�������Ă݂܂��傤.

  line(gp.cut('lon'=>135,'lat'=>35), true, 'exchange'=>true)  # �c�����������W�ɂ��邽�߂� exchange ��ݒ肵�Ă��܂�.

���ς��邽�߂̃��\�b�h mean ������܂�.
�����ɑя󕽋� (���������ɕ���) �������x���z�����Ă݂܂��傤.

  set_fig('itr'=>1)   # ���W�n�����ɖ߂��Ă���.
  tone(gp.mean('lon'))

==== ���K���

(1) 500 hPa �f�ʂ̉��x���z��`���Ă݂܂��傤.

=== GPhys �I�u�W�F�N�g�̑��� ���� 2

����񂪋��ʂ��� GPhys �I�u�W�F�N�g���m�ł�
NArray �̗v�̂Ōv�Z���ł��܂�.

  eddy = gp - gp.mean('lon')  # �я󕽋ς���̕΍�
  tone(eddy)
  color_bar

�`��ȊO�ɂ����p���Ă݂܂��傤.
���{�t�߂� 1000 hPa �̉��x�𒲂ׂĂ݂܂��傤.

  gp2 = gp.cut('lon'=>135,'lat'=>35,'level'=>1000))

# �����̃��\�b�h�� "-5.5... degC" �ƕW���o�͂����͂�. ���@�Y�ꂽ.

GPhys �I�u�W�F�N�g�ɂ͎���P�ʂ̏�񂪓����Ă��܂�.
�l���������o�������ꍇ��
�ȉ��̂悤�ɂ���� NArray �`���̃f�[�^�������܂�.

  gp2.val

���Ȃ݂Ɏ��̍��W�̒l�����o���ɂ͂��̂悤�ɂ��܂�.

  gp.axis('level').pos.val

==== ���K���

(1) 1000 hPa, �ܓx 30 �x�ɂ�����я󕽋ω��x�����߂Ă݂܂��傤.

(2) (����) 70 hPa, �o�x 135 �x�f�ʂ�, ��ԋC�����Ⴂ�ܓx�͂ǂ��ł��傤��.

=== �e�L�X�g���o��

�f�[�^���e�L�X�g�ŏo�͂��Ă݂܂�.
�����Ƃ��Ĉȉ��̓��e����͂��Ă����܂�.

  na_jpn = gp2.val
  level = gp.axis('level').pos.val

��̓s���̂��߂�, �f�[�^���l�ߑւ��Ă����܂�.

  n = na_jpn.size   # �z��̃T�C�Y�𓾂�
  nary = NArray.sfloat(n,2)    # sfloat �͒P���x����
  nary[true,0] = level         # true �͂��̎����̑S�v�f���Ӗ�����
  nary[true,1] = na_jpn.to_a   # NArrayMiss �N���X�Ȃ̂ł��̂܂ܑ���ł��Ȃ�. �T���v���̖��?

�e�L�X�g�f�[�^�Ƃ��ď����o���Ă݂܂��傤.

  file = File.open("test.dat", 'w')   # test.dat �Ƃ������̃t�@�C�����������ݗp�ɊJ��
  (0..n-1).each{|i| 
    file.puts nary[true, i].to_a.join(" ")  # ���p�X�y�[�X������ŘA����, ������Ƃ��ďo��
  }
  file.close    # �t�@�C�������

�[���Œ������Ă݂�

  $ cat test.dat

���x��, ���������o�͂����t�@�C����ǂݍ���ł݂܂��傤.

  file2 = File.open("test.dat", 'r')   # �ǂݍ��ݗp�ŊJ��
  str = file2.read     # �t�@�C���̓��e�S�Ă𕶎���Ƃ��� str �ɑ��
  file2.close

���ꂾ�� str �͂܂��A�����ꂽ������Ȃ̂�, 
�����₷���悤�ɔz��ɂ�, NArray �ɕϊ����܂�.

  ary = str.split("\n").each{|a| a = a.split(" ") }
  nary = NArray.to_na(ary)

split ��, �h�b�g�̑O�̕�����������̕�����ŕ�����, �z��ɕϊ����郁�\�b�h�ł�.

=== GPhys �I�u�W�F�N�g�̍쐬

������ GPhys �I�u�W�F�N�g������Ă݂܂��傤.
�������ǂݍ��񂾃f�[�^�𗘗p���܂�.

  level = nary[true,0]
  temp  = nary[true,1]

���łɃf�[�^�̒��g��ύX���Ă݂܂�.
���̉��x�f�[�^�̒P�ʂ͐ێ��Ȃ̂Ő�Ή��x�ɂ��Ă݂܂�.

  temp = temp + 273.15

GPhys �I�u�W�F�N�g�����܂�
(�����̂ŃR�s�[�y�[�X�g����).

  # �����
  va_level = VArray.new( level,
                      {"long_name"=>"Level", "units"=>"hPa"},
                      "level" )
  axis_level = Axis.new.set_pos(va_level)
  
  # ���x
  data = VArray.new( temp,
                     {"long_name"=>"temperature", "units"=>"K"},  # �P�ʂ� K �ł��邱�Ƃɒ���
                     "T" )

  # GPhys �I�u�W�F�N�g�̍쐬
  gp3 = GPhys.new( Grid.new(axis_level), data )

�`�悵�Ă݂܂��傤.

  line(gp3, true, 'exchange'=>true)

���x�̎��̐��l��P�ʂ��������ƕς���Ă��܂�.

==== ���K���

(1) ���x�f�[�^�� "long_name"=>"temperature" �� "temperature" ���D���ȕ�����ɕς���
    GPhys �I�u�W�F�N�g���쐬����, �}�̃^�C�g���⎲�̖��O���ς�邱�Ƃ��m�F���Ă݂܂��傤.

=== netCDF �t�@�C���̓��o��

GPhys �I�u�W�F�N�g�� netCDF �t�@�C���ɏ��������Ă݂܂�.

  outfile = NetCDF.create("test.nc")
  GPhys::IO.write(outfile, gp3)  # netCDF �t�@�C���ɏ����o��
  outfile.close

�Ƃ����, ����܂ł�

  gp = gpopen('T.nc/T')

�Ƃ������ T.nc �̒��ɕϐ� T �����邱�Ƃ�m���Ă���
netCDF �t�@�C����ǂݍ���ł��܂�����, 
���̐l���z�z���Ă���t�@�C���͂����Ȃ��Ă���Ƃ͌���܂���.
�t�@�C���̒��ɂǂ̂悤�ȕϐ������邩���ׂĂ݂܂��傤.

  GPhys::IO.var_names('T.nc')

# ���̃��\�b�h�� alias �����ق���������������Ȃ�.

==== ���K���

(1) test.nc ��ǂݍ����, ���R�ɕ`�悵�Ă݂܂��傤.

=== �X�N���v�g�t�@�C���ōs���ɂ�

irb �͎��s����Ɍ����Ă��܂���, 
���܂����}���ʂɐ���������, ���G�ȑ��������̂ɂ͌����Ă��܂���.
���̏ꍇ�̓X�N���v�g�t�@�C���ɂ��Ă�����
���ƂŎg���񂹂�̂ŕ֗��ł�.

�������߂� irb �̂Ƃ��Ɠ����ł�.
�P���ȗ�������܂�.

  #!/usr/bin/env ruby
  require "irb_ggraph"     # ���܂��Ȃ�. �J�����g�f�B���N�g���� irb_ggraph.rb ���Ȃ��ƃG���[�ɂȂ�̂Œ���
  gp = gpopen('T.nc/T')
  tone(gp)
  DCL.grcls   # �X�N���v�g�t�@�C���ɂ���Ƃ��͕K�{

���Ƃ��΂���� test.rb �Ƃ��ĕۑ����܂�.
���s�͂��̂悤�ɂ��܂�.

  $ ruby test.rb

�t�@�C���Ɏ��s�����������, ���Z���R�}���h�ɂȂ�܂�.

  $ chmod u+x test.rb
  $ ./test.rb

==== ���K���

(1) test.rb �̒��g���D���ɏ�������, ���s���Ă݂܂��傤.

(2) test.rb �̏o�͐�� netCDF �t�@�C���ɂ��Ă݂܂��傤.

(3) test.rb �Ŏg���f�[�^���e�L�X�g�t�@�C������̓��͂ɂ�����, 
    �t�@�C����ǂݍ��܂��Ɏ茳�Ő��������f�[�^�ɂ��Ă݂܂��傤.
    (�q���g: �ŏ��� NArray �Ŏ��ƃf�[�^�����,
    ����� GPhys �I�u�W�F�N�g�ɕϊ�����)

== �Q�l����

* ((<"GPhys/GGraph �`�[�g�V�[�g"|URL:http://davis.gfd-dennou.org/rubygadgets/ja/?%28Others%29+GPhys%2FGGraph+%A5%C1%A1%BC%A5%C8%A5%B7%A1%BC%A5%C8>))
* GPhys ���t�@�����X�}�j���A��
  (
  ((<"�p��"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
  ((<"���{���"|URL:http://w.livedoor.jp/gphys/>))
  )



== �t�^: ���ۂ̎g�p��

=== �T���v��

* ((<"RubyDCL demo programs"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/demo/>))

=== ���ۂ̌����Ɏg���Ă����
# �{���̓`���[�g���A���ɂ͕s�v����, �d�]���i�̐�`��, 
# ���ۂɎg���Ă����������邱�Ƃɂ��Q���҂̃��`�x�[�V�����グ��_����.

* ��C��z���f���̗�: ((<"dcpam5 ��p�����v�Z�̌���"|URL:http://www.gfd-dennou.org/library/dcpam/sample/>))
* ��×͊w���f��: ((<"�v���O�����̃e�X�g�v�Z [deepconv/arare5]"|URL:http://www.gfd-dennou.org/library/deepconv/arare/sample/>))
* (���f�������łȂ��ϑ����ʂȂǂ̐}�W���~����)


