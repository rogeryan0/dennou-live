= �V���ȃf�[�^��������������

=== �ڎ�

* ((<GPhys�g���̊�b>))
* ((<�z�ɍ��W�X�V��K�v�Ƃ���ꍇ>))
* ((<Ruby�ł͒x�����鏈����C�ŏ���>))

== GPhys�g���̊�b

((*����*)): 
NCEP �ĉ�͂̓�����
((<uwnd.2010.nc|URL:uwnd.2010.nc>)) �̓����ϒl�f�[�^
((-���̃f�[�^��((<NOAA�̃T�C�g|URL:http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html>))����擾���܂����D�{�t�@�C���ɂ�1-2���̃f�[�^�̂ݓ����Ă܂��D-)) 
��K���ȍ�ƃf�B���N�g���Ƀ_�E�����[�h���C������ cd ���܂��D


Ruby�ł͊����̃N���X�ɁC���\�b�h����ǉ�������Ē�`���邱�Ƃ��ȒP�ɂł��܂��D
���̂��߁CGPhys �ɂȂ��@�\���ȒP�ɕt�������ė��p�ł��܂��D

GPhys �N���X�́CNumRu �Ƃ������W���[�����ɒ�`����Ă��܂��D������g���C�Ē�`����ɂ́C�܂��C
(('numru/gphys')) �� (('require')) ��
�i���邢�́C�`����s���ꍇ�� (('numru/ggraph')) �� (('require')) �� --
�� (('numru/ggraph')) �͓����� (('numru/gphys')) �� (('require')) ����j�C
���̂悤�ɑ����܂��i�r���ɖ��֌W�ȃR�[�h���͂���ł��\���܂���j�D
�����ŁC (('...')) �ɂ́C���\�b�h��`���������܂��D

  module NumRu
    class GPhys
      ...
    end
  end

���\�b�h��ǉ�����T���v���v���O�����������܂����F

�T���v���v���O���� ((<variance.rb|URL:variance.rb>)):
  require "numru/ggraph"

  iws = (ARGV[0] || 1).to_i

  module NumRu
    class GPhys
      def variance(*dims)
        dev = self - self.mean(*dims)
        variance = (dev**2).mean(*dims)
        variance.long_name = "variance: "+long_name if variance.is_a?(GPhys)
        variance
      end
    end
  end

  include NumRu

  u = GPhys::IO.open("uwnd.2010.nc","uwnd")[false,{0..30,3}]  # Jan 1,4,..,31

  p "variance (all data points):",u.variance

  DCL.swpset('iwidth',900)
  DCL.swpset('iheight',450)
  DCL.swpset('ldump',true) if iws==4
  DCL.gropn(iws)
  DCL.sldiv('y',2,1)
  DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
  DCL.glpset('lmiss',true)

  GGraph.set_fig "itr"=>2, "viewport"=>[0.15,0.75,0.2,0.8]
  GGraph.tone u.variance(0).mean(-1), true, "int"=>50
  GGraph.color_bar

  GGraph.set_fig "itr"=>10, "viewport"=>[0.05,0.8,0.2,0.8]
  GGraph.set_map "coast_world"=>true
  GGraph.tone u.cut("level"=>850..200).variance("level").mean("time")
  GGraph.color_bar

  DCL.grcls

((*���s����*))

�W���o��(�ꕔ):
  "variance (all data points):"
  260.16754165344 m2 s-2

�}: ((<(full size)|URL:variance.png>)) :

((<"IMG:variance_th.jpg">))


��̗�ł̓o���A���X���v�Z���郁�\�b�h (({variance})) ���`���܂����D
(({def variance(*dims)})) �Ƃ����`�Œ�`���Ă���̂ŁC�C�ӌ̈������Ƃ邱�Ƃ��ł��܂��D
����́C

        dev = self - self.mean(*dims)
        variance = (dev**2).mean(*dims)

�ɂ����āC(({mean})) �Ƃ������\�b�h�ɂ��̂܂܈����p����܂��D
(({mean})) �́C�ΏۂƂȂ� GPhys �I�u�W�F�N�g�̎�����C�ӌC���O�܂��͔ԍ��Łi�[���ȏ�̐����őO����C���̐����Ō�납��j�w�肷��ƁC�����̎����ɉ��������ς�Ԃ����\�b�h�ł��D
�������Ȃ��ꍇ�́C���ׂẴf�[�^�_���g�������ς��X�J���[�l (�P�ʕt���̐��l UNumeric) 
�ŕԂ��܂��D
�����`����  (({variance})) �́C���̏_������̂܂܈����p���܂��D
�Ȃ��C(({mean})) �͊i�q�_�̊Ԋu���l�����Ȃ��P�����ςł� (�������C�f�[�^�����͏����ăJ�E���g���܂�) �̂ŁC
�s���Ԋu�ȃf�[�^�ɓK�p����ꍇ�͂��̓_���ӎ����Ă����K�v������܂��D�i�q�_�Ԋu���l�����镽�ςɂ�
(({average})) ������܂��D

�f�[�^���������\�b�h���Ē�`���Ă����ƁC���������ɕʂ̃��\�b�h�Ɉ����p���u���\�b�h�`�F�[���v�Ŏg���闘�_������܂��D��̗�ł́C�}������ۂɂ͎��̂悤�Ɏ��ԕ��ς��Ƃ��Ă܂��F

  GGraph.tone u.cut("level"=>850..200).variance("level").mean("time")


== �z�ɍ��W�X�V��K�v�Ƃ���ꍇ

((*����*)): 
NCEP �ĉ�͂̓�����
((<uwnd.2010.nc|URL:uwnd.2010.nc>)) �̓����ϒl�f�[�^
((-���̃f�[�^��((<NOAA�̃T�C�g|URL:http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html>))����擾���܂����D�{�t�@�C���ɂ�1-2���̃f�[�^�̂ݓ����Ă܂��D-)) 
��K���ȍ�ƃf�B���N�g���Ƀ_�E�����[�h���C������ cd ���܂��D


�X�y�N�g����͂��s���ꍇ���l���Ă݂܂��傤�D�t�[���G�ϊ��ł͎���Ԃ���g����ԂɈڍs����̂ŁC���W���̕ύX�͕K�{�ł��DGPhys�ɂ̓t�[���G��̓��C�u�������܂܂�Ă��܂����C(('gphys_fft.rb')) 
�Ƃ����Ɨ������t�@�C���Ɋi�[����Ă���C�܂��ɑO�߂ŏq�ׂ�GPhys�ǉ���`�Ɠ����`�ɂȂ��Ă��܂��D�]���āC���̃t�@�C���͍��W�̍X�V��K�v�Ƃ���g���̈�T���v���t�@�C���Ƃ��Ă݂邱�Ƃ��ł��܂��D�\�[�X�R�[�h�́CGPhys 
�̃\�[�X�t�@�C����W�J�����g�b�v�f�B���N�g���̉��C�܂��� GPhys �C���X�g�[����g�b�v�f�B���N�g���̉��́C (('lib/numru/gphys/gphys_fft.rb')) �Ƃ����p�X�ɂ���܂��D

���āC(('gphys_fft.rb')) �̍��W���X�V�͑�|����ȗ�ł��̂ŁC���܂�Ƃ����₷���Ȃ���������܂���D�����ŁC�����ƊȒP�ȗ�Ƃ��āC�����@�Ŕ�����]�����郁�\�b�h��ǉ������������܂��D

�T���v���v���O���� ((<deriv.rb|URL:deriv.rb>)):
  require "numru/ggraph"
  iws = (ARGV[0] || 1).to_i

  module NumRu
    class GPhys
      def deriv(dim)
        dim = dim_index(dim)    # input dim can be a String or an Integer
        x = axis(dim).to_gphys
        a = [true]*dim + [1..-1,false]
        b = [true]*dim + [0..-2,false]
        dydx = ( self[*a] - self[*b] ) / ( x[1..-1] - x[0..-2] )
        xi = ( x[1..-1] + x[0..-2] ) / 2
        dydx.axis(dim).set_pos(xi.coord(0))
        dydx.long_name = "d #{name}/d #{x.name}"
        dydx
      end
    end
  end

  include NumRu

  u = GPhys::IO.open("uwnd.2010.nc","uwnd")[false,0].cut("lon"=>140)

  ushear_p = u.deriv("level")

  H = UNumeric[8e3,"m"]
  z = -H * ( u.axis("level").pos / 1000.0 ).log
  z.name = "z"
  z.long_name = "log-p height"
  z.set_att("positive","up")
  u.axis("level").set_pos(z)

  ushear_z = u.deriv(-1)

  DCL.swpset('iwidth',800)
  DCL.swpset('iheight',400)
  DCL.swpset('ldump',true) if iws==4
  DCL.gropn(iws)
  DCL.sldiv('y',2,1)
  DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
  DCL.glpset('lmiss',true)
  GGraph.set_fig "itr"=>2, "viewport"=>[0.15,0.75,0.2,0.8]
  GGraph.tone_and_contour ushear_p
  GGraph.color_bar
  GGraph.set_fig "itr"=>1
  GGraph.tone_and_contour ushear_z
  GGraph.color_bar
  DCL.grcls

((*���s����*))

�}: ((<(full size)|URL:deriv.png>)) :

((<"IMG:deriv_th.jpg">))


���̗�ł́C(({deriv})) �Ƃ������\�b�h���`���Ă��܂��D
���̃��\�b�h�́C�ԍ��܂��͖��O�Ŏw�肳�ꂽ�����ɉ����āC�ׂ荇���i�q�_�l�����Ƃɍ�����]�����܂��D���̍ہC���ʂ̍��W�𗼎҂̒����ɐݒ肷�邱�ƂŁC���������ƂȂ�悤�ɂȂ��Ă��܂��D

(({deriv})) �̓��e������ǂ��Đ������܂��D

        dim = dim_index(dim)    # input dim can be a String or an Integer

�ł́C�Y�����鎟���̔ԍ����擾���܂��D���͂�������ł���Ύ������Ɣ��f���Ĕԍ� (0,1,..) ��Ԃ��܂��D�܂��C���͂����̐����ł���΁C��납��̎w��ł���Ƃ݂Ȃ��C0,1,.. �̔ԍ��Ɋ��Z���܂��D

        x = axis(dim).to_gphys

�����ł́Cdim �Ŗڂ̍��W�����Ƃ�C(('to_gphys')) �Ŏ���W�ϐ� (�i�q�_�̍��W�l�����߂�1���� VArray) ����ϐ�(�f�[�^�l)�Ƃ��� 
GPhys �ɂ��Ă��܂��i�܂� x �͈ꎟ���ŁC��ϐ���1�����ڂ̎���W�ϐ�����v���܂��j�D������ GPhys
�ɂ��闝�R�́C���Ƃő������f�[�^�Ƃ̏����v�Z���邽�߂ł��DGPhys �ǂ����̌v�Z�ł���΁C�����̑Ή��Â������O�x�[�X�Ŏ����I�ɍs����̂ŁC�������Έꎟ���̉��Z���\�Ȃ̂ł��D

        a = [true]*dim + [1..-1,false]
        b = [true]*dim + [0..-2,false]
        dydx = ( self[*a] - self[*b] ) / ( x[1..-1] - x[0..-2] )

�����ł́C���Ƃ��� dim �� 2 �ł���΁C(('a')) �� (('[true,true,1..-1,false]')),
(('b')) ��  (('[true,true,0..-2,false]')) �ƂȂ�܂��D����āC(('self[*a]')) 
��3�Ԗڂ̎����ɂ��āC2�Ԗڂ̗v�f����Ō�̗v�f�܂ł��Ƃ����T�u�Z�b�g�C(('self[*b]')) 
��3�Ԗڂ̎����ɂ��āC�擪�̗v�f����C��납��2�Ԗڂ̗v�f�܂ł��Ƃ����T�u�Z�b�g�ƂȂ�܂��D
�]���āCdydx �� dim ���ɉ����ėׂ����i�q�_�ԂłƂ��������ɂȂ�̂ł��D

�������C���̂܂܂ł́Cdydx �� dim ���́C���Z���ӂ� (('self[*a]')) �̂���Ɠ����܂܂ł��DGPhys ��2�����Z�ł͍��W�͍��ӂ�������p���d�l�ɂȂ��Ă��邩��ł��D���̂܂܂ł���ލ����Ƃ��Ă͂悢�̂ł����C���������Ƃ��ׂ��i�q�̈ʒu�𒆉��ɂ����Ă����܂��D���ꂪ�ȉ��ł��D

        xi = ( x[1..-1] + x[0..-2] ) / 2
        dydx.axis(dim).set_pos(xi.coord(0))

�����ł� (('xi.coord(0)')) �i�ŏ��̎��̎���W�ϐ��j�Ƃ��邩����  (('xi.data')) �i��ϐ��j�Ƃ��Ă������ł��D���Ƃ͎d�グ�ł��D

        dydx.long_name = "d #{name}/d #{x.name}"

�ł͋L�q�I�Ȗ��O��\���\�񑮐� (('"long_name"')) �������܂��i������ (('name')) ���g���̂��K�؂Ƃ͌���܂��񂪁C(('long_name'))
���Ȍ��ł��邱�Ƃ����҂��Ă܂��D�ėp���\�b�h���Ă͂��܂�悭�Ȃ���������܂���j�D

�ȏ�Ń��\�b�h (({deriv})) �̐������I���܂����D���͂��̗��p�ɂ��āC�ȒP�ɐ������܂��D
��̃T���v���v���O�����ł́C�܂��͂��Ƃ��Ƃ̉������W���i�C���G���O��(�Ȃ���)level�j�ɂ��āC�������Ƃ�܂��G

  ushear_p = u.deriv("level")

���ŁC���x�͉������W�����͑ΐ����W�ɂ��������ŁC�������Ƃ�܂��D���W����ւ��́C

  u.axis("level").set_pos(z)

�ōs���܂����C(({u})) �Ƃ��� GPhys �I�u�W�F�N�g�ɂ�������W�������ւ��Ă��邾���Ȃ̂ŁC���Ƃ̃t�@�C���ɂ͉e�����y�ڂ��܂���D�i���Ƃ̃t�@�C����������������@������܂����C�f�t�H���g�ł̓t�@�C���͓ǂݍ��݂݂̂̏����o���֎~�ŃI�[�v������܂��j�D����āC���ɉ������W�Ŕ�������ƈ��͑ΐ����W�ł̔����ƂȂ�܂��D


== Ruby�ł͒x�����鏈����C�ŏ���

�i���݂܂���C���쐬�ł��D�j
