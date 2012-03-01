# = irbrc_ggraph.rb
# irb �� GPhys ���g���₷������X�^�[�g�A�b�v�t�@�C��.
# �z�[���f�B���N�g�������ɂ����Ďg���Ƃ悢�D
# 
# == �g�p�@
#   $ irb -r ~/irbrc_ggraph.rb
#
# .bashrc ���ŁC���̂悤�� alias ���Ă����ƁCirb_ggraph �Ƃ����R�}���h����
# �Ăׂ�悤�ɂȂ�
#   alias irb_ggraph="irb -r ~/irbrc_ggraph.rb"
#
# ���邢�́C�z�[���f�B���N�g���� .irbrc ��
#      require "~/irbrc_ggraph.rb"
# �Ə����ƁCirb �R�}���h�N�����ɏ�ɓǂݍ��܂��悤�ɂȂ�D
#
# == �@�\
# ������g���� ls �� dir �Ƃ��������\�b�h�Ńf�B���N�g����f�[�^�t�@�C����
# ���邱�Ƃ��ł���悤�ɂȂ�D�܂��C�f�B���N�g����f�[�^�t�@�C���� cd �ł���
# �悤�ɂȂ� (GPhys���\�ȃf�[�^���܂ރt�@�C���̓f�B���N�g���Ɠ��l�Ɉ�����).
#
# �� (irb �ւ̓���):
#  dir
#  dir "air.mon.ltm.nc"
#
# GPhys �I�u�W�F�N�g�̓O���[�o�����\�b�h gpopen �ŊJ���D
#
# ��:
#  temp = gpopen("air.mon.ltm.nc/air")
# 
# irb �̃q�X�g���[���ꗗ������ (�O���[�o�����\�b�h history),
# �t�@�C���ɕۑ�������ł���悤�ɂȂ� (�O���[�o�����\�b�h history_save).
# �f�t�H���g�̕ۑ��ꏊ�� ~/irb_ggraph_history.rb �Ƃ����t�@�C���D
# (�f�t�H���g�͒ǋL)


print "Start interactive GGraph session\n"

require "numru/ggraph"
include NumRu
include GGraph

### GPhys�̐ݒ�D���Ƀf�B���N�g���֘A�֗̕��ȃ��\�b�h��` ###

GDir.top='/'
GDir.cd(Dir.pwd)
def pwd; GDir.pwd; end                       # pwd �Ō��݂̃f�B���N�g���\��
def cwd; GDir.cwd; end                       # (���܂�g��Ȃ�)
def ls(path=nil); GDir.cwd.ls(path); end     # ls �Ńf�B���N�g�����e�\��(�Z)
def ls_l(path=nil); GDir.cwd.ls_l(path); end # ls_l �Ńf�B���N�g�����e�\��(��)
alias dir ls_l                               # dir �� ls_l �̕ʖ�
def cd(path); GDir.cd(path); end             # cd �Ńf�B���N�g���ύX. �����͕�����
                                             # �Ȃ̂� cd "T.jan.nc" �ȂǂƂ���
def gpopen(path); GDir.cwd.data(path); end   # �t�@�C�����̃f�[�^��� GPhys �I�[�v��.
                                             # ��: gp = gpopen "T.jan.nc/T"
def gpopen_all; GDir.cwd.open_all_data; end  # �t�@�C����cd���Ă���ĂԂ�GPhys���\
                                             # �ȑS�f�[�^���n�b�V���ɓǂݍ��ށD
                                             # (�ϐ����̃V���{�����L�[)
### �`�� (GGraph, DCL) �̐ݒ� ###

DCL.swpset('iwidth',700)    # �f�t�H���g�̉摜�T�C�Y�����i�D�݂ŕҏW����Ƃ悢�j
DCL.swpset('iheight',700)   # �f�t�H���g�̉摜�T�C�Y�����i�D�݂ŕҏW����Ƃ悢�j
DCL.swpset('lwait',false) if $0 == "irb"  # don't wait mouse click to renew page
##DCL.swpset('ldump',true)  # �摜�_���v�D���u�ԍ�4�̂Ƃ��� PNG �ŁD
##DCL.swpset('lalt',true)   # �����`��D�p���p���A�j���p�D�ʏ�Θb�����ɂ͌����Ȃ��D
DCL.sgscmn(10)              # �J���[�}�b�v�ԍ���10�ԂɁDgropn�O�Ȃ�Ď�։�

##DCL.gropn(1)         # �ʏ�͍ŏ��̕`�掞�Ɏ����I�ɌĂ΂��D
                       # ���u�ԍ�1�ԈȊO���g���ꍇ��y�[�W��������ۂ͗z�ɌĂ�
##DCL.sldiv('y',2,2)   # �y�[�W�����̗�D�y�[�W��������ꍇ�C��� DCL.gropn(1) ��z�ɌĂ�

DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
DCL.glpset('lmiss',true)

#DCL.sgscmn �Őݒ肷��J���[�}�b�v�ԍ��̃���
# 
# 1:  dcl_original
# 2:  black-orange-yellow-white
# 3:  black-blue-cyan-white
# 4:  blue-cyan-white-yellow-red
# 5:  gray_scale
# 6:  pastel_rainbow
# 7:  black-rainbow-black
# 8:  white_yellow_red
# 9:  white_blue_black
# 10: short_green_original
# 11: black-rainbow-white
# 12: pink-rainbow-pink

### History �@�\ ###

def history(latest_n=nil)     # block accpeted
  if latest_n
    if !block_given?
      (-latest_n..-1).each{|i| puts Readline::HISTORY[i]}
    else
      (-latest_n..-1).each{|i| yield(Readline::HISTORY[i])}
    end
  else
    if !block_given?
      Readline::HISTORY.each{|s| puts s}
    else
      Readline::HISTORY.each{|s| yield(s)}
    end
  end
  Readline::HISTORY.length
end

def history_grep(regexp)
  history{|h| puts h if regexp =~ h}
end

def history_save(path=File.expand_path("~/irb_ggraph_history.rb"),renew=false)
  if renew
    mode = "w"
  else
    mode = "a"
  end
  file = File.open(path,mode)
  file.print("###########################\n## #{Time.now.to_s}\n")
  history{|s| file.puts(s)}
  file.close
  print "irb history saved in ",path,"\n"
  nil
end