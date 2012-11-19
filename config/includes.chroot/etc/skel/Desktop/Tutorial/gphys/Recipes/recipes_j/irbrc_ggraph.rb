# = irbrc_ggraph.rb
# irb �� GPhys ���g���₷������X�^�[�g�A�b�v�t�@�C��.
# �z�[���f�B���N�g�������ɂ����Ďg���D
# 
# == �g�p�@
#   $ irb -r ~/irbrc_ggraph.rb
#
# .bashrc ���ŁC���̂悤�� alias ���Ă����ƁCirb_ggraph �Ƃ����R�}���h����
# �Ăׂ�悤�ɂȂ�
#   alias irb_ggraph="irb -r ~/irbrc_ggraph.rb"
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
# �O���[�o�����\�b�h open �� GPhys �I�u�W�F�N�g���J���悤�ɍĒ�`�����D
#
# ��:
#  temp = open("air.mon.ltm.nc/air")
# 
# irb �̃q�X�g���[���ꗗ������ (�O���[�o�����\�b�h history),
# �t�@�C���ɕۑ�������ł���悤�ɂȂ� (�O���[�o�����\�b�h history_save).
# �f�t�H���g�̕ۑ��ꏊ�� ~/irb_ggraph_history.rb �Ƃ����t�@�C���D

print "Start interactive GGraph session\n"

### History ###

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

### for GGraph ###

require "numru/ggraph"
include NumRu
include GGraph

GDir.top='/'
GDir.cd(Dir.pwd)

def cwd; GDir.cwd; end
def pwd; GDir.pwd; end
def ls(path=nil); GDir.cwd.ls(path); end
def ls_l(path=nil); GDir.cwd.ls_l(path); end
alias dir ls_l
def cd(path); GDir.cd(path); end
def open_all_data; GDir.cwd.open_all_data; end  # --> Hash of GPhys objs in cwd
def data(path); GDir.cwd.data(path); end        # --> GPhys
alias open data

DCL.swpset('iwidth',700)
DCL.swpset('iheight',700)
DCL.swpset('lwait',false)   # don't wait mouse click to show the next page
##DCL.swpset('ldump',true)  # useful if wsn==4 (dump png files)
##DCL.swpset('lalt',true)   # background plot
DCL.sgscmn(10)              # set colomap (see the memo below)

##DCL.gropn(1)
##DCL.sldiv('y',2,2)

DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
DCL.glpset('lmiss',true)

=begin
MEMO

color map numbers:
 1:  dcl_original
 2:  black-orange-yellow-white
 3:  black-blue-cyan-white
 4:  blue-cyan-white-yellow-red
 5:  gray_scale
 6:  pastel_rainbow
 7:  black-rainbow-black
 8:  white_yellow_red
 9:  white_blue_black
 10: short_green_original
 11: black-rainbow-white
 12: pink-rainbow-pink
=end
