#! ruby -Ks
# -*- coding: japanese-cp932 -*-
# [����]
# �T���v���X�N���v�g���ɓ��{����g�p����ꍇ�A
# �X�N���v�g�̍ŏ��ɏ�L�̋L�q���s��Ȃ���
# ���������̗v���ƂȂ�B
###########################################################################
#
# -*- coding: cp932 -*-
#
###########################################################################
# = knowledge_sample_creating_document.rb
#   Gfdnavi �� Web�T�[�r�X��p���Ēm���������쐬����v���O�����B
#   * Gfdnavi�C���X�g�[�����Ɋ܂܂��T���v���f�[�^��p����
#     �摜���쐬���A���̉摜���܂߂��m��������ۑ�����
#   * �ȒP�ȕ`��I�v�V����������w�肵�č�}���s���A
#     ���̐}���܂߂��������쐬����T���v���B
###########################################################################
# == ���s�ɕK�v�Ȋ�
#    * Ruby 1.8
#    * gfdnavi_data
#      http://www.gfd-dennou.org/arch/davis/gfdnavi/download/
#      ���擾�ł���B
#      $ ruby install.rb
#      �Ƃ��ăC���X�g�[������B
###########################################################################

# == �͂��߂�
require "numru/gfdnavi_data"
require "pp"
include NumRu

# === �w���v���o�͂ł���悤��
if ARGV.include?("-h") || ARGV.include?("--help")
  print "USAGE: ruby #$0 [OPTIONS] [gfdnavi_webservice_portal_URL]\n"
  print "OPTIONS: \n"
  print "   -u [user_name] : If this option is none, interpreted as \"root\".\n"
  print "   -h             : Display this help.\n"
  print "gfdnavi_webservice_portal_URL: \n"
  print "   ex) http://0.0.0.0:3000/data        <- default \n"
  exit
end

# === ���̃T���v���v���O������p���āAroot �ȊO�̃��[�U�Ƃ���
#     �h�L�������g���쐬����Ƃ��ɂ̓I�v�V�����ŃA�J�E���g�����w�肷��
#     (�w�肷��A�J�E���g���A�ڑ����� Gfdnavi ��ɍ쐬����Ă���K�v������)
if (u_option = ARGV.index("-u"))
  user = ARGV[u_option +1]
  ARGV.delete_at u_option
  ARGV.delete_at u_option
else
  user = "root"
end

# === URL �̏���
url_prefix = ARGV.shift || "http://0.0.0.0:3000/data"
url_prefix = url_prefix.sub(/\/\z/, "")

# === �摜�� imagemagick �𗘗p���ĕ\�����邽�߂̃��\�b�h
def display(plot)
  IO.popen("display","w"){|io| io.print(plot.to_png)}
end

###########################################################################
# == ��������A�����쐬�̃T���v��
###########################################################################

# === �m���������쐬����
# ���߂ɁAGfdnaviData::KnowledgeRemote �I�u�W�F�N�g���쐬����K�v������
nk = GfdnaviData::KnowledgeRemote.new
# ������ۑ�����p�X���w�肷��(�K�{)
nk.url         = url_prefix + "/usr/#{user}/knowledge/knowledge_sample_creating_document.knlge"
# �^�C�g�����w�肷��(�K�{)
nk.title       = "Temperature data from ncep!!!"
# �쐬�҂��w�肷��(�K�{)
nk.user        = user
# �����̃J�e�S�����w�肷��(�K�{)
nk.category    = "memo"
# �\�������u���Җ��v���w�肷��
nk.creator     = "Davis Taro"
# �����̗v����w�肷��
nk.description = "Australia is warm."
# �{���̓��e���L�q����
#nk.textbody    = "The most heated area in Figure 1 is a point of 130 degrees of east longitude, 20 degrees of south latitude.\nThat is, it's Australia."
nk.textbody    = "ncep�̃f�[�^������((<figure 1>))���쐬�����B\n
����͑S���̒n�\�ʂ̉��x���v���b�g�������̂ł���B\n
���̐}�����Ă݂�ƁA�ł��C���̍����A�s���N�F�ɕ\�����ꂽ���������邱�Ƃ�������B\n
�����ŁA�n�}���e���s���A�ǂ̂悤�Ȓn��ł��邩�𒲂ׂĂ݂邱�Ƃɂ���(((<figure 2>)))�B\n
���X���Â炢���A���̍����̒n��̓I�[�X�g�����A�Ɋ܂܂�Ă��邱�Ƃ�������B\n
�܂��A((<figure 3>))�ɂ��̒n����g�債�ĕ\�����Ă݂��B\n
���S����32�x�𒴂��邱�Ƃ����������B\n
\n\n
((<figure 4>))�͖k�ɓ_�𒆐S�Ƃ����n�}���e�ł���B\n
���̐}������ƁA�ɂ���ԓ��Ɍ������āA�ܓx���Ⴍ�Ȃ�ɏ]���C�����Ⴍ�Ȃ��Ă����l�q���悭������B\n
"

# === �����ɓ��ꂽ���}���쐬����B
# �܂��AGfdnaviData.open ����K�v������
t = GfdnaviData.open(url_prefix + "/samples/reanalysis/ncep/T.jan.nc/T")

# ����ꂽ GfdnaviData::VariableRemote �I�u�W�F�N�g�ɑ΂���
# plot ���\�b�h���g�����ƂŁA�`�悳���B
image01 = t.plot("tone_contour")
# �I�v�V�����̎w��ɂ́A�n�b�V����p����B�f�t�H���g�l�ɂ��Ă͂��̃t�@�C���̍Ō�Ɏ����B
# (�I�v�V�����̏ڍׂ� http://ruby.gfd-dennou.org/products/gphys/doc/ggraph.html ���Q��)

# �n�}���e(�����J�g���}�@)
image02 = t.plot("tone_contour", {"projection"=>11})

# �\���͈͂����肵�Ă݂�
opts = {
  "axes"=>
  {"lon"=>{"max"=>180, "min"=>100},
    "level"=>{"min"=>"1000"},
    "lat"=>{"max"=>"0", "min"=>"-45"}}, # ���̎w����s��(�͈͂̎w��������čs��)
  "x_axis"=>"lon",  
  "y_axis"=>"lat",  
  "z_axis"=>"level"
}
image03 = t.plot("tone_contour", opts)
#display image03
# ,
#  "color_bar"=>true # �J���[�o�[�̕\��


# polar steleo projection.
opts = {
  "projection"=>31
}
image04 = t.plot("tone_contour", opts)

# === �쐬�����}�𕶏��ɉ�����B
#     add_figure= ���\�b�h�́A�����ɐ}��1�ǉ�����B
#     (������ plot �̌��ʂƁA�L���v�V����)
nk.add_figure(image01, "�n�\�ʂ̉��x")
nk.add_figure(image02, "figure 1��n�}�ɓ��e����(�����J�g���}�@)")
#nk.add_figure(image03, "�������̂ݑ傫���\������") ### �Ȃ�image03 �����I�v�V�����̂������G���[���o��B�v���؁B
nk.add_figure(image04, "�k�ɂ𒆐S�Ƃ����n�}���e")


# === �������̐}��(�f�t�H���g��)���C�A�E�g��ݒ肷��
#     0: "layout_figures_under_text"
#     1: "layout_one_figure_above_text"
#     2: "layout_figures_in_a_row_above_text"
#     �f�t�H���g�� 0.
nk.default_layout = 0

# ���C�A�E�g�� "layout_figures_under_text"�̂Ƃ��̂݁A
# �u���ɉ������ׂ邩�v���w�肷��B(�f�t�H���g��1��)
# (�w�肵���������z�������͎��̍s�ɕ���)
nk.horizontal_figures = 2


# === �}�̑傫�����܂Ƃ߂Ďw�肷��B
# (1) �u�������A���̂ǂ�����w�肷�邩�v���w�肷��B
#     0: height 
#     1: width
#     �f�t�H���g�� 0.
nk.figures_size_height_or_width = 0

# (2) �u�s�N�Z���P�ʂł̎w�肩�A���X�̑傫���ɑ΂���{���ł̎w�肩�v���w�肷��B
#     0: %
#     1: px
#     �f�t�H���g�� 0.
nk.figures_size_units = 0

# (3) ���l���w�肷��B(1), (2) �ƕ����ĉ��߂���A�\�������}�̑傫�������܂�B
#     �f�t�H���g�� 100.
nk.figures_size_number = 120


# === �ۑ�����Bsave ���\�b�h��p����B
if nk.save
  print "Save is successful.\n"
end
exit

# �`��I�v�V�����̃f�t�H���g�l
=begin
opts = {"coloring"=>false, # true/false
  "color_bar"=>false,      # true/false
  "log"=>false,            # true/false
  "exchange"=>false,       # true/false
  "variables_order"=>"", # ?
  "projection"=>1,       # ���l
  "map_fit"=>true,        # true/false
  "contour"=>true,        # true/false
  "tone"=>true,           # true/false
  "axes"=>
  {"lon"=>{"max"=>"350", "min"=>"0"},
    "level"=>{"min"=>"1000"},
    "lat"=>{"max"=>"-90", "min"=>"90"}},
  "x_axis"=>"lon",       # ������
  "y_axis"=>"lat",       # ������
  "z_axis"=>"level",     # ������
  "viewport"=>["0.2,0.8,0.2,0.8"],              # ���̂Ƃ���͕�����(�z��ɂ����URL�ɔ��p�X�y�[�X��������)
  "map_window"=>["-180.0,180.0,-75.0,75.0"]}    # ���̂Ƃ���͕�����
=end

#  "viewport"=>[0.2,0.8,0.2,0.8],
#  "map_window"=>[-180.0,180.0,-75.0,75.0]}

# exchange : true or 1 or "1" �Ȃ� true ����
# map_fit  : ���܂������Ă���̂��ǂ����悭������Ȃ�
# viewport : X���AY�����ꂼ��̕\���͈͂�ݒ肷��B


