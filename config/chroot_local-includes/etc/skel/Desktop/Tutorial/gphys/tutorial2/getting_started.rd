== �͂��߂�O��

�{�`���[�g���A�����n�߂�O�̏����ł��D

==== �T�v

* GPhys/GGraph�p�X�^�[�g�A�b�v�t�@�C�� ((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) ���_�E�����[�h���Cirb �𗧂��グ�Ă݂܂��D
* �T���v���Ƃ��Ďg���C�ۃf�[�^
  ((<ncep2.Jan.clim.1981-2010.nc|URL:ncep2.Jan.clim.1981-2010.nc>)),
  ((<air.2012-01.nc|URL:air.2012-01.nc>)),
  ((<hgt.2012-01.nc|URL:hgt.2012-01.nc>))
  ���_�E�����[�h���܂��D

=== _

=== �C���X�g�[��

GPhys �͑������f�[�^��͉������C�u�����ł��DGGraph �� GPhys �t���̉������C�u�����ł��D

���̃`���[�g���A���́CGPhys �̓C���X�g�[�����Ă�����̂Ƃ��Đi�߂܂��D
�������܂��Ȃ� ((<GPhys�z�[���y�[�W|URL:http://ruby.gfd-dennou.org/products/gphys/>))
���Q�l�ɃC���X�g�[�����Ă��������D
##�Ȃ� LiveDVD �ɂ̓C���X�g�[���ς݂ł��D

=== Ruby �̎��s

Ruby �ŏ����v���O�����͒ʏ�C.rb �Ƃ����g���q�ŏI���t�@�C���ɕۑ����C
ruby �R�}���h�Ŏ��s���܂��D�Ⴆ�� hoge.rb �Ƃ������O�̃t�@�C���ɕۑ������v���O�����́C
�R�}���h���C���[����

  $ ruby hoge.rb

�Ǝ��s���܂��D�����ł̓v�����v�g�� $ �ŕ\���܂����i�Ȍ�����l�ɕ\���܂��j�D

���̕��@�Ƃ��āC���̂悤�ɑΘb�I�C���^�[�v���^�[�ɂ����s���ł��܂��D

=== irb �ɂ���

irb �� Ruby �̑Θb�I�C���^�[�v���^�[�ł��D
�{�`���[�g���A���ł� irb �𑽂��g���܂��D

�R�}���h���C���[���� irb �𗧂��グ���,
1�s���͂���x��Ruby�R�[�h�Ƃ��Ď��s����C���ʂ��\������܂�.
irb �𗧂��グ��ɂ́C�� irb �Ɠ��͂��܂��D
����ƁC���͑҂��̃v�����v�g�i(('irb(main):001:0>')) �Ƃ�����������j���\�������̂ŁC�Ƃ肠�����l�����Z�ł�����Ă݂܂��傤("(('1 + 2'))"�����[�U�[�̓��͂ł�)�D

  $ irb
  Irb(main):001:0> 1 + 2
  => 3

#(�ҏW��) ��ł̓v�����v�g�擪�啶�����ŃL�[�v

==== irb �̏����ݒ�Ɨ����グ���D

�Θb�I�����p�ݒ�t�@�C�� ((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) ���_�E�����[�h���ăz�[���f�B���N�g���ɒu���܂� 
(�z�[���f�B���N�g���� UNIX �n OS �ł� ~/ �ƕ\���܂�. 
Windows���ł͓K���ȃt�H���_�[�ɒu���C�ȉ��� ~/ �͂���ɓǂݑւ��Ă�������).

���̐ݒ�t�@�C����ǂݍ���� irb �𗧂��グ��ɂ�

  $ irb -r ~/irbrc_ggraph.rb

�Ƃ��܂��D���񂱂���^�C�v����͖̂ʓ|�ł��̂ŁC���� 1, 2 �̂����ꂩ�ɂ���āC�����ƊȒP�Ɏg����悤�ɂ��܂��傤�D

(1) �X�^�[�g�A�b�v�t�@�C���ւ̓o�^�D

    �z�[���f�B���N�g���� .irbrc �Ƃ����t�@�C�������C���g�Ɏ��̍s�������܂��D

      require "~/irbrc_ggraph.rb"

    ����ƁC�P�ɃR�}���h���C���� irb �Ɠ��͂��邾���� irbrc_ggraph.rb ���ǂݍ��܂�܂��D

(2) �ʖ��̐ݒ�@(linux�Ȃ�UNIX�n��OS�̂�)

    �������K�v�ȂƂ����� irbrc_ggraph.rb ��ǂݍ��ނ悤�ɂ�������΁C�ʖ���ݒ肵�܂��傤�D
    �Ⴆ�� bash ���g���Ă�ꍇ�C~/.bashrc �Ɏ��̍s�������܂��D

       alias irb_ggraph="irb -r ~/irbrc_ggraph.rb"

    ����ŁC

      $ irb_ggraph

    �Ƃ���� irbrc_ggraph.rb ���ǂݍ���� irb ���n�܂�܂�.
    �ʖ���o�^���Ă����΃R�}���h���̃^�u�⊮�������܂��̂ŁC���O��S���o���ĂȂ��Ă����v�ł�.

�{�`���[�g���A���ł� (2) �̕��@���g���܂��D(1) �̕��@���g���ꍇ�C
(('$ irb_ggraph')) �Ə����Ă���� (('$ irb')) �Ɠǂݑւ��Ă��������D

=== �T���v���f�[�^�̃_�E�����[�h

�{�`���[�g���A���ł͎��̃f�[�^���g���܂��D
�_�E�����[�h���ēK���ȃf�B���N�g���[�i�t�H���_�[�j�ɂ����Ă��������D
�`���[�g���A���͂��ׂĂ��̃f�B���N�g���[�Ŏ��s���܂��D

* ((<ncep2.Jan.clim.1981-2010.nc|URL:ncep2.Jan.clim.1981-2010.nc>)) : 1981�N����2010�N�܂ł�30�N�Ԃ̃f�[�^��蓾��ꂽ�����̋C��l�i�u���N�l�v�j�ł��D���^�����ʂ͎��ł��i�R�����̑O�̓t�@�C�����̕ϐ����ł��j�D
  * (('mslp')) : �C�ʍX���C�� (Pa)
  * (('air')) : �C�� (K)
  * (('hgt')) : �����ʍ��x (m)
  * (('uwnd')) : ������ (m/s)
  * (('vwnd')) : ��k�� (m/s)
* ((<air.2012-01.nc|URL:air.2012-01.nc>)) : 2012�N1���̋C���̓����ϒl(K)�ł��D���^�ϐ����� (('air'))
* ((<hgt.2012-01.nc|URL:hgt.2012-01.nc>)) : 2012�N1���̓����ʍ��x�̓����ϒl(m)�ł��D���^�ϐ����� (('hgt'))

������ ((<NCEP-DOE Rananalysis2|URL:http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis2.html>))
�ɂ��q�ω��(�ĉ��)�f�[�^�ŁC�t�@�C���`���� ((<NetCDF|URL:http://www.unidata.ucar.edu/software/netcdf/>)) �ł��D

��������o�x�ܓx2.5�x�̓��Ԋu�f�[�^�ŁCmslp �ȊO�͉����ɂ͋C�����W�̃f�[�^�ƂȂ�܂�(1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 
100, 70, 50, 30, 20, 10 hPa ��17�w)�D1000 hPa �Ȃǂ́u�n�ʂ̉��v�i�܂���ۂɂ͂��̋C���̉ӏ��͑��݂��Ȃ��j�Ƃ���������ł����C���̏ꍇ��O�ƂȂ��Ă��܂��i�������ɂ�`�x�b�g�����ł� 1000 hPa �́u�C���v����`����Ă��܂��j�D

�����̃t�@�C���͏�L�̃T�C�g����擾�ł���f�[�^�� ((<������|URL:tool/>)) 
�ɒu���� Ruby �v���O�����ŉ��H���č��܂����iGPhys���g���Ă��܂��D�Ȃ�1���̋C��l�݂̂̐؂�o���͑Θb�I�ɍs���܂����j�D

=== NetCDF �f�[�^�̓��e�m�F

�R�}���h���C���� NetCDF �f�[�^�̓��e���m�F����ɂ� ncdump �R�}���h���g���܂�
�i�I�v�V���� -h �����܂��j�D�ȉ���2�s�ڈȍ~�� ncdump �̏o�͂ł��D

  $ ncdump -h air.2012-01.nc 
  netcdf air.2012-01 {
  dimensions:
          lon = 144 ;
          lat = 73 ;
          level = 17 ;
          time = 31 ;
  variables:
          float lon(lon) ;
                  lon:units = "degrees_east" ;
                  lon:long_name = "Longitude" ;
                  lon:actual_range = 0.f, 357.5f ;
                  lon:standard_name = "longitude" ;
                  lon:axis = "X" ;
          float lat(lat) ;
                  lat:units = "degrees_north" ;
                  lat:actual_range = 90.f, -90.f ;
                  lat:long_name = "Latitude" ;
                  lat:standard_name = "latitude" ;
                  lat:axis = "Y" ;
          float level(level) ;
                  level:units = "millibar" ;
                  level:actual_range = 1000.f, 10.f ;
                  level:long_name = "Level" ;
                  level:positive = "down" ;
                  level:GRIB_id = 100s ;
                  level:GRIB_name = "hPa" ;
                  level:axis = "Z" ;
          double time(time) ;
                  time:units = "hours since 1-1-1 00:00:0.0" ;
                  time:long_name = "Time" ;
                  time:actual_range = 17628096., 17629344. ;
                  time:delta_t = "0000-00-01 00:00:00" ;
                  time:avg_period = "0000-00-01 00:00:00" ;
                  time:standard_name = "time" ;
                  time:axis = "T" ;
          short air(time, level, lat, lon) ;
                  air:long_name = "mean Daily Air temperature" ;
                  air:unpacked_valid_range = 150.f, 350.f ;
                  air:actual_range = 185.3f, 314.275f ;
                  air:units = "degK" ;
                  air:add_offset = 477.66f ;
                  air:scale_factor = 0.01f ;
                  air:missing_value = 32766s ;
                  air:precision = 2s ;
                  air:least_significant_digit = 1s ;
                  air:GRIB_id = 11s ;
                  air:GRIB_name = "TMP" ;
                  air:var_desc = "Air temperature" ;
                  air:dataset = "NCEP Reanalysis Daily Averages" ;
                  air:level_desc = "Multiple levels" ;
                  air:statistic = "Mean" ;
                  air:parent_stat = "Individual Obs" ;
                  air:valid_range = -32766s, -12766s ;
  
  // global attributes:
                  :Conventions = "COARDS" ;
                  :title = "mean daily NMC reanalysis (2012)" ;
                  :history = "created 2011/12 by Hoop (netCDF2.3)\n",
                          "2012-02-28 18:40:29 JST horinout> extractJan.rb wrote air" ;
                  :description = "Data is from NMC initialized reanalysis\n",
                          "(4x/day).  It consists of most variables interpolated to\n",
                          "pressure surfaces from model (sigma) surfaces." ;
                  :platform = "Model" ;
                  :references = "http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html" ;
  }
