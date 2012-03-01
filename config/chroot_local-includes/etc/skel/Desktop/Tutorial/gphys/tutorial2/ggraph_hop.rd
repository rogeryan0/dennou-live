# GPhys, GGraph �`���[�g���A�� / �����̑���
#
# = ���� (�V�������̂͏ォ��ǋL)
#
# * 2012/02/29 �x�V����  �܂��܂��ҏW��
# * 2012/02/26 �x�V����  �ҏW�J�n�i�啝�ύX�j
# * 2012/02/18 �[���N�j  �V�K�쐬 (hop_ggraph.rd �Ƃ���)

= GPhys, GGraph�`���[�g���A�� (����2)

== �����̑���

���̏͂́C�`�惉�C�u�����ł��� GGraph (GPhys�̕t�����C�u����)���g�����C�ۃf�[�^�̉����̃`���[�g���A���ł�.
Ruby�̑Θb�I�C���^�[�v���^�[ irb ���g���āC�C�ۃf�[�^�̊G���ȒP�ɕ`���邱�Ƃ�̌����܂��D

�O������ ((<�u�͂��߂�O�Ɂv|URL:getting_started.htm>)) �̏͂ŏЉ�����������Ă����Ă��������D

((:<div class=summary>:))
==== ���̏͂̊T�v

* �ő�2�s�ŊG���`���邱�Ƃ�̌����܂��i�f�[�^�I�[�v��1�s�C�`��1�s�j�D
* �������̎�ނ̕`�������Ă݂܂��i�F�h��C�R���^�[�C�܂��...�j�D�`��ݒ�̏�����̌����܂��D
* ���킹�Ď��̂悤�Ȃ��Ƃ��w�т܂��F�G���[���b�Z�[�W�̓ǂݕ��C�q�X�g���[�@�\ (�g�p����irb�X�^�[�g�A�b�v�t�@�C���̋@�\)�D
((:</div>:))

=== �ő��`��

�[���� irb �� GPhys �p�̃X�^�[�g�A�b�v�t�@�C����ǂݍ��񂾌`�ŗ����グ�܂��i((<�u�͂��߂�O�Ɂv|URL:getting_started.htm>)) �Q��; (('~/.irbrc')) �̐ݒ�����Ă�ꍇ�͈ȉ��� irb_ggraph �łȂ� irb �Ƒł����ށj�D

  $ irb_ggraph

�܂��͈ȉ����v�����v�g�i(('irb(main):001:0>'))�Ȃǁj�ɑ����ē��͂��Ă݂܂��傤.
'#' �ȍ~�̓R�����g���Ȃ̂œ��͂���K�v�͂���܂���i���͂��Ă���肠��܂���j.

# �ȉ��ł̓R�s�[���y�[�X�g�̕ւ��l���āCirb �̃v�����v�g�i(('irb(main):001:0>')) ���j�͕\���������[�U�[�̓��͂𑾕����ŕ\�����Ƃɂ��܂��D

  irb(main):001:0> gp = gpopen 'air.2012-01.nc/air'   # �f�[�^��ǂݍ���
  irb(main):002:0> tone gp                # �g�[���Ƃ��ĕ`��

����ƁC�E�B���h�[�������オ�莟�̉摜���\������܂��D

((:<a href="img/ggraph_hop_irb01.png">:))
((<"IMG:img/ggraph_hop_irb01_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

air.2012-01.nc �̒��̕����� air (�C��) �̃f�[�^��
�o�x�C�ܓx�C�C���C���Ԃ� 4 �����ł���, GGraph �����܂����� 2 �����f�ʂ����܂��D
3�����߈ȍ~�͍ŏ��̗v�f (�ŉ��w���ŏ��̎���) ���Ƃ�C�o�x�ܓx�f�ʂ�`���܂�
(���̒f�ʂ̕`������((<���̏�|URL:ggraph_step.htm>))�ŏЉ�܂�)�D
���W����^�C�g���̓f�[�^����ǂݎ���Ă��C���ŕ\������܂��D

gp �ւ̑����, ���̓��e�ŏ㏑�������� irb ���I�������肵�Ȃ�����,
�ēx�s���K�v�͂���܂���.

�Ƃ���ŁC��� gpopen ���Ă񂾍ہC���̂悤�Ƀ��b�Z�[�W����ʂɕ\�����ꂽ�͂��ł��D

  irb(main):002:0> gp = gpopen 'air.2012-01.nc/air'   # �f�[�^��ǂݍ���
  => <GPhys grid=<4D grid <axis pos=<'lon' in '/home/username/air.2012-01.nc'  sfloat[144]>>
          <axis pos=<'lat' in '/home/username/air.2012-01.nc'  sfloat[73]>>
          <axis pos=<'level' in '/home/username/air.2012-01.nc'  sfloat[17]>>
          <axis pos=<'time' in '/home/username/air.2012-01.nc'  float[31]>>>
     data=<'air' in '/home/username/air.2012-01.nc'  sint[144, 73, 17, 31]>>

��1�s�ڂ̖`���� (('<GPhys')) �Ƃ���܂��D����́C
gp �Ƃ����ϐ��ŕ\����郂�m (object) ���CGPhys �Ƃ����u�N���X�v(�u�^�v�Ǝv���΂����ł�)�̃��m 
(object)�ł��邱�Ƃ�\���܂��D�������e����́C���ꂪ�ǂ̂悤�ȃf�[�^���\���Ă��邩�C������x�z���ł���ł��傤�D

=== �G���[���o����

�R�}���h������đł��Ă��܂����Ƃ��܂��傤.

  irb(main):003:0> gp = gpopen "air.2012-01.nc/airrr"  # air �ƊԈႦ�� airrr �Ƒł��Ă��܂��� ^^;
  RuntimeError: variable 'airrr' not found in #<NumRu::NetCDF:0x0000000255a5d0>
        from /usr/lib/ruby/1.8/numru/gphys/gphys_netcdf_io.rb:528:in `__files2varray'
        from /usr/lib/ruby/1.8/numru/gphys/gphys_netcdf_io.rb:317:in `open'
        from /usr/lib/ruby/1.8/numru/gphys/gphys_io.rb:121:in `open'
        from /usr/lib/ruby/1.8/numru/gdir.rb:572:in `data'
        from /usr/lib/ruby/1.8/numru/gdir.rb:566:in `data'
        from /home/username/irbrc_ggraph.rb:53:in `gpopen'
        from (irb):3

�p��ŃG���[����ʂɏo�͂���ēǂݔ�΂������Ȃ邩������܂���,
�G���[���C�����邽�߂̃q���g�������邱�Ƃ������̂ł�������ǂ݂܂��傤.
�܂�����ׂ��� 1 �s�ڂł�.
variable 'airrr' not found �ł�����, �u�ϐ� `airrr' ��������Ȃ��v�ł���.
2�s�ڂ���̓G���[�����������ӏ�����Ăяo�����Ƃɑk��u�g���[�X�o�b�N�v�ł��D
����ׂ��u�匳�v�̓G���[�̈�ԉ��̍s�ł��D��ł� 
from (irb):3 ���Ȃ킿�Cirb �� 3 �s�ڂ̓��͂ƂȂ�܂��D
from �̂��Ƃ̐����͍s�� (irb �̏ꍇ�̓v�����v�g�E�̂ق��ɕ\������Ă���ԍ�) �ł�.
���̏ꍇ������񒼑O�̓��͂ł��D�����܂œǂ߂΋��炭�������ł��邩�v������ł��傤�D
���C�u�����̃o�O���^����Έ�s����Ɍ��čs���܂���,
�قƂ�ǂ̏ꍇ�͎������������ꏊ (�G���[�̉��̕�) �Ɍ���������܂��̂�, 
�ނ�݂ɏ�ɍs���Ȃ��ق��������ł�.

==== ���K���

(1) �E�ӂ� (('gp = gpopen "airrr.2012-01.nc/air"')) 
    �ƕύX���ăG���[���o���Ă݂܂��傤 (�t�@�C�������ԈႦ��).
    �����ăG���[���b�Z�[�W����ǂ��Ă݂܂��傤.

=== �����Ȏ�ނ̐}��`���Ă݂�

GGraph �ɂ͗l�X�ȕ`�惁�\�b�h���p�ӂ���Ă��܂�.

==== 1�����`��

�܂��, �}�[�N

  irb(main):004:0> line(gp)
  irb(main):004:0> mark(gp)

����(�k�ɂł́u�o�x�f�ʁv�Ȃ̂ň�l�ł܂�Ȃ��ł���)�F

((:<a href="img/ggraph_hop_irb02.png">:))
((<"IMG:img/ggraph_hop_irb02_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))
((:<a href="img/ggraph_hop_irb03.png">:))
((<"IMG:img/ggraph_hop_irb03_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))


==== 2�����`��

�g�[�� (�ŏ��̗�Ɠ��������J���[�o�[������)

  irb(main):004:0> tone(gp)
  irb(main):004:0> color_bar

���ʁF
((:<a href="img/ggraph_hop_irb03.5.png">:))
((<"IMG:img/ggraph_hop_irb03.5_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

�R���^�[

  irb(main):004:0> contour(gp)

���ʁF
((:<a href="img/ggraph_hop_irb04.png">:))
((<"IMG:img/ggraph_hop_irb04_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

�g�[���ƃR���^�[�̏d�˕`��

  irb(main):004:0> tone_and_contour(gp)

���ʁF
((:<a href="img/ggraph_hop_irb05.png">:))
((<"IMG:img/ggraph_hop_irb05_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

==== ���̑�

�U�z�} (scatter), �F���U�z�}(color_scatter),
�x�N�g���} (vector) ������܂�. 
#�i�܂��C���� ganalysis
#�Ƃ��� GPhys �t�����C�u�����ɓ����Ă���q�X�g�O�����`��� GGraph �Ɏ�荞�܂��\��ł��D�j
�ڂ����̓��t�@�����X�}�j���A��
(
((<"�p��"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
((<"���{���"|URL:http://w.livedoor.jp/gphys/>))
)
���Q�Ƃ�������.

=== �J�b�R����������Ȃ�������...

Ruby �ł̓��\�b�h�i���֐��Ǝv���Ă����ł��j�̈����͊ۊ��ʂŊ���܂���,
���ꂪ�Ȃ���Ώȗ��ł��܂��D�Ⴆ�Ύ���2�͓����ł��D

  irb(main):004:0> tone(gp)
  irb(main):004:0> tone gp

�D�݂łǂ�����g���Ă��悢�ł��Dirb�őΘb�I�Ɏg���ꍇ�C�^�C�v�̏��Ȃ���҂��኱���₷���悤�Ɏv���܂����C��ŏo�Ă���悤�Ƀ��\�b�h���Ȃ���ꍇ���ʂ͌������܂���D

=== �`��I�v�V���� (1)

�}�̕`�������J�X�^�}�C�Y���邱�Ƃ��ł��܂�.

���W�n, �n�}���e��ύX���Ă݂܂��傤.

  irb(main):004:0> set_fig 'itr'=>10             # �����~���}�@
  irb(main):004:0> set_map 'coast_world'=>true   # �n���̊C�ݐ���\��
  irb(main):004:0> tone gp

���ʁF
((:<a href="img/ggraph_hop_irb06.png">:))
((<"IMG:img/ggraph_hop_irb06_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

���Ȃ݂�

  irb(main):004:0> set_fig 'itr'=>10             # �����~���}�@

����͂����ۂ̕W���o�͂�, 

  => {'itr'=>1}

�ł���͂��ł�.
����͂���܂łɐݒ肳��Ă����l�ł�.
���ɖ߂������Ƃ��͂��̒l���������Ă����܂��傤.

���Ԃ��ǂ̍��W�n�Ɋ��蓖�Ă��Ă��邩��
((<"���낢��Ȓn�}���e�@"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (�炭�炭 DCL ��)
�� 
((<DCL �}�j���A���́u���W�n�̎�ށv|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>))
���Q�l�ɂ��Ă�������.

#�����, �J���[�o�[��\����, �������̖ڐ��� 10 �x����,
#������ 30 �x���݂ɏ����悤�ɂ��܂�.
#
#  set_fig('itr' => 1 )    # ���W�n�����ɖ߂�
#  color_bar               # �J���[�o�[�̕\��
#  set_axes('xtickint' => 10, 'xlabelint' => 30)  # �ڐ��̐ݒ�
#  tone(gp)


���x�͖k�ɂ���݂Ă݂܂��傤�D

  irb(main):004:0> set_fig('itr'=>30)            # ���ː}�@
  irb(main):004:0> tone(gp)

���ʁF
((:<a href="img/ggraph_hop_irb07.png">:))
((<"IMG:img/ggraph_hop_irb07_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

�F�̓h��� (�J���[�}�b�v) �̕ύX�͂������܂�.

  irb(main):004:0> DCL.sgscmn(3)   # 3 �Ԃ̃J���[�}�b�v(��-��-���̃O���f�[�V����)���g�p
  irb(main):004:0> DCL.gropn(1)    # �V�����`�摋�̕\�� (DCL 5.4.4�ȍ~�͕s�v�ɂȂ�͂�)
  irb(main):004:0>  tone(gp)

���ʁF
((:<a href="img/ggraph_hop_irb08.png">:))
((<"IMG:img/ggraph_hop_irb08_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))


(('DCL.')) �Ŏn�܂�̂� ((<RubyDCL|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/>)) 
�̃��\�b�h�ł��DRubyDCL ��
((<DCL|URL:http://www.gfd-dennou.org/library/dcl/>))
�� Ruby ����Ăׂ�悤�ɂ������̂ł��D
GGraph �� RubyDCL �Ƌ������Ďg���悤�ɂł��Ă��܂��D
�Ȃ��Cirb �ɓǂݍ���ł���X�^�[�g�A�b�v�t�@�C�� 
((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>))
�ɂ� (('DCL.')) �Ŏn�܂�R�[�����������񂠂邱�Ƃ��킩��ł��傤�D

(('DCL.sgscmn')) �� RubyDCL �̃��\�b�h�ŁC�J���[�}�b�v�����ւ��܂�.
���Ԃ̃J���[�}�b�v���ǂ������F�̓h����ɑΉ����邩��
((<�u�炭�炭 DCL�v�̕`���|URL:http://www.gfd-dennou.org/library/dcl/dcl-5.4.2/src/env1/colormap/colormap_gallery.html>))
���Q�l�ɂ��Ă�������. �C���X�g�[������Ă��� DCL ���o�[�W������ 5.4.4 ���O�̏ꍇ�C
�J���[�}�b�v�̎��ւ��� gropn �ɂ��f�o�C�X�̏������O�ɍs���K�v������܂��D
(('DCL.gropn(1)')) �̍s�͂��̂��߂ł��D

==== ���K���

(1) tone �̐}�������̍D���ȃJ���[�}�b�v�ɕύX���Ă݂܂��傤.

##[�[��] �͂܂肻���Ȃ̂ň�U�R�����g�A�E�g. �]�T������Ƃ��Ɍ�����.
#(2) (����) GPhys ���t�@�����X�}�j���A��
#    (
#    ((<"�p��"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
#    ((<"���{���"|URL:http://w.livedoor.jp/gphys/>))
#    ) ���Q�l��, �c���̖ڐ��ł����ύX���Ă݂܂��傤.
#    (�q���g: ���g���Ă���̂� GGraph �Ȃ̂ł܂��y�[�W���� "GGraph" �Ō��������
#    ����炵���y�[�W�ւ̃����N��������.
#    ����, �����̖ڐ��� set_axes �ŕύX���Ă������Ƃ���
#    �s������̃y�[�W�� "set_axes" �Ō��������...)

=== �ŁCGGraph �͂ǂ��ɂłĂ����́H �i�ǂݔ�΂��Ă�OK�j

����܂ł̘b�ł́CGGraph �Ƃ������̂����Ȃ̂��悭������Ȃ��ł��傤�D
GGraph �́CRuby �̗p��ł̓��W���[���Ƃ����J�e�S���[�ɂȂ�C
�����ȃ��\�b�h�𑩂˂����̂ƂȂ��Ă��܂��D
����܂Ő������Ă� tone �Ȃǂ� GGraph �̃��\�b�h�Ȃ̂ł��D
���ꂪ�z�ɂ킩��悤�ɌĂԂɂ́C

  irb(main):004:0> GGraph.tone(gp)

�ȂǂƌĂт܂��D�����炪�ނ��됳���ȌĂѕ��Ȃ̂ł����C
�X�^�[�g�A�b�v�t�@�C�� ((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) 
�ɂ����čŏ��� GGraph. ���ȗ��ł���悤�ɐݒ肵�Ă���̂ł��D
DCL �����W���[���ł��̂ŁCGGraph. ���ȗ����Ȃ��ق����Ώ̐����悢�ł��D
irb ���g�킸�v���O�������t�@�C���ɂ��� GGraph �𗘗p����ۂɂ́C
GGraph. ���ȗ����Ȃ����Ƃ����߂܂��i��قǂ��̃��b�X�������܂��j�D

=== �q�X�g���@�\

�����Ȃ�܂����̂ŁC������ň�U irb ���I�����܂��傤�D
���̑O�ɓ��͓��e��ۑ����Ă݂܂��傤�D
((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) �ɂ͕֗��ȃq�X�g���@�\����`����Ă��܂�
�i�����݂�Ε�����܂����C���s�Ŏ�������Ă��܂��j�D
irb �� (('history')) �Ɠ��͂��Ă݂Ă��������D

  irb(main):021:0> history
  gp = gpopen 'air.2012-01.nc/air' 
  tone gp
  gp = gpopen "air.2012-01.nc/airrr"  # air �ƊԈႦ�� airrr �Ƒł��Ă��܂��� ^^;
  line(gp)
  ..(�㗪)..

(('history_save')) �Ɠ��͂���Ɨ������ۑ�����܂��D

  irb(main):022:0> history_save
  irb history saved in /home/username/irb_ggraph_history.rb
  => nil

�����̓z�[���f�B���N�g�������� ((<irb_ggraph_history.rb|URL:irb_ggraph_history.rb>))
�Ƃ����t�@�C���ɕۑ�����܂��D(('history_save')) 
�������Ȃ��ŌĂԂƁC�����͂��̃t�@�C���ɒǋL����Ă����܂��D

irb ���I������ɂ� (('exit')) �Ɠ��͂��܂��D

  irb(main):022:0> exit


=== ���̏͂ň��p��������

* GPhys ���t�@�����X�}�j���A��
  (
  ((<"�p��"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
  ((<"���{���"|URL:http://w.livedoor.jp/gphys/>))
  )

* DCL���T�|�[�g������W�n
  * ((<"���낢��Ȓn�}���e�@"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (�炭�炭 DCL ��)
  * ((<DCL �}�j���A���́u���W�n�̎�ށv|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>))

* DCL
  * ((<DCL�z�[���y�[�W|URL:http://www.gfd-dennou.org/library/dcl/>))
  * ((<RubyDCL|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/>)) (DCL��Ruby�Ŏg����悤�ɂ�������)
  * �J���[�}�b�v�F ((<�u�炭�炭 DCL�v�̕`���|URL:http://www.gfd-dennou.org/library/dcl/dcl-5.4.2/src/env1/colormap/colormap_gallery.html>))
