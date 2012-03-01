# GPhys, GGraph �`���[�g���A�� (�z�b�v. GGraph ��)
#
# = ���� (�V�������̂͏ォ��ǋL)
#
# * 2012/02/18 �[���N�j  �V�K�쐬
#

((*���̃h�L�������g�͍쐬���ł�. �w�K�ɂ�
((<"���s�̃`���[�g���A��"|URL:http://ruby.gfd-dennou.org/products/gphys/tutorial/>))
���Q�Ƃ�������.*))


== �����̊�b

���̏͂́C�`�惉�C�u�����ł��� GGraph (GPhys�̕t�����C�u����)���g�����C�ۃf�[�^�̉����̃`���[�g���A���ł�.
GGraph ���g���ƊȒP�ɉ����ł��邱�Ƃ�̌����܂��D

�O������ ((<�u�͂��߂�O�Ɂv|URL:getting_started.htm>)) �̏͂ŏЉ�����������Ă����Ă��������D

=== �`��

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
3�����߈ȍ~�͍ŏ��̗v�f (�ŉ��w���ŏ��̎���) ���Ƃ�C�o�x�ܓx�f�ʂ�`���܂��D

((* ���̒f�ʂ̕`�����͌�قǎ����܂��D*))

gp �ւ̑����, 
���̓��e�ŏ㏑�������� irb ���I�������肵�Ȃ�����,
�ēx�s���K�v�͂���܂���.

=== �G���[���o����

������R�}���h��ł��Ă��܂����Ƃ��܂��傤.

  irb(main):003:0> gp = gpopen("air.2012-01.nc/airrr")  # air �ƊԈႦ�� airrr �Ƒł��Ă��܂���
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
�]���Č���ׂ��u�匳�v��, �G���[�̈�ԉ��̍s�ł��D��ł� 
from (irb):3 ���Ȃ킿�Cirb �� 3 �s�ڂ̓��͂ƂȂ�܂��D
from �̂��Ƃ̐����͍s�� (irb �̏ꍇ�̓v�����v�g�E�̂ق��ɕ\������Ă���ԍ�) �ł�.
������񒼑O�̓��͂ł��D�����܂œǂ߂Ή������ł��邩�v������ł��傤�D
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

�g�[�� (�ŏ��̗�Ɠ���)

  irb(main):004:0> tone(gp)

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

=== �`��I�v�V���� (1)

�}�̕`�������J�X�^�}�C�Y���邱�Ƃ��ł��܂�.

���W�n, �n�}���e��ύX���Ă݂܂��傤.

  irb(main):004:0> set_fig('itr'=>10 )            # �����~���}�@
  irb(main):004:0> set_map('coast_world'=>true)   # �n���̊C�ݐ���\��
  irb(main):004:0> tone(gp)

���ʁF
((:<a href="img/ggraph_hop_irb06.png">:))
((<"IMG:img/ggraph_hop_irb06_s.png">))�i�N���b�N�Ńt���T�C�Y�\���j
((:</a>:))

���Ȃ݂�

  irb(main):004:0> set_fig('itr'=>10 )            # �����~���}�@

����͂����ۂ̕W���o�͂�, 

  => {'itr'=>1}

�ł���͂��ł�.
����͂���܂łɐݒ肳��Ă����l�ł�.
���ɖ߂������Ƃ��͂��̒l���������Ă����܂��傤.

���Ԃ��ǂ̍��W�n�Ɋ��蓖�Ă��Ă��邩��
((<"���낢��Ȓn�}���e�@"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (�炭�炭 DCL ��)
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

  DCL.sgscmn(3)   # 3 �Ԃ̃J���[�}�b�v (��-��-���̃O���f�[�V����) ���g�p
  DCL.gropn(1)    # �V�����`�摋�̕\�� (DCL 5.4.4 �ȍ~�͕s�v�ɂȂ�͂�)
  tone(gp)

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
�� 
((<DCL �̃}�j���A��|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>))
���Q�l�ɂ��Ă�������. �C���X�g�[������Ă��� DCL ���o�[�W������ 5.4.4 ���O�̏ꍇ�C
�J���[�}�b�v�̎��ւ��� gropn �ɂ��f�o�C�X�̏������O�ɍs���K�v������܂��D
(('DCL.gropn(1)')) �̍s�͂��̂��߂ł��D

==== ���K���

(1) tone �̐}�������̍D���ȃJ���[�}�b�v�ɕύX���Ă݂܂��傤.

###############
((* 2012-02-28 �x�V��. ���݂����܂ō�� *))
###############


# �͂܂肻���Ȃ̂ň�U�R�����g�A�E�g. �]�T������Ƃ��Ɍ�����.
#(2) (����) GPhys ���t�@�����X�}�j���A��
#    (
#    ((<"�p��"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
#    ((<"���{���"|URL:http://w.livedoor.jp/gphys/>))
#    ) ���Q�l��, �c���̖ڐ��ł����ύX���Ă݂܂��傤.
#    (�q���g: ���g���Ă���̂� GGraph �Ȃ̂ł܂��y�[�W���� "GGraph" �Ō��������
#    ����炵���y�[�W�ւ̃����N��������.
#    ����, �����̖ڐ��� set_axes �ŕύX���Ă������Ƃ���
#    �s������̃y�[�W�� "set_axes" �Ō��������...)

=== �`��I�v�V���� (2)

�}�͏d�˂������邱�Ƃ��ł��܂�.

  tone(gp)
  contour(gp, false)  # �O�̐}�������Ȃ��ł��̏�ɏd�˕`��

���������ȗ������ꍇ�� true (�O�̐}�������Đ}��`������) �Ɠ��������ɂȂ�܂�.

�}�̃J�X�^�}�C�Y�ł���, 
�`�惁�\�b�h�ɂ���Ă�, set_fig �̂悤�ȃ��\�b�h�łȂ�, 
�`�惁�\�b�h�̈����ɂ���ĕύX���邱�Ƃ��ł��܂�.

  contour(gp, true, 'interval'=>5)

���̂Ƃ�, �������͏ȗ��ł��Ȃ��̂Œ��ӂ��Ă�������.


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
* ((<"RubyDCL �h�L�������g"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/>))
* ((<"���낢��Ȓn�}���e�@"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (RubyDCL �h�L�������g��)
* ((<"DCL colormaps"|URL:http://www.gfd-dennou.org/library/dcl/dcl-5.4.2/src/env1/colormap/colormap_gallery.html>)) (DCL �h�L�������g��)
