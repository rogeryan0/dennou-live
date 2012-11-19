# -*- coding: japanese-cp932 -*-
# = knowledge_sample_reading_document.rb
#   Gfdnavi �� Web�T�[�r�X��p���Ēm��������ǂݎ��A
#   ��X�̏���W���o�͂ɕ\������T���v���v���O����
#   * ���O�C���̕K�v�͖���
#   * �ǂݎ��̂́AGfdnavi�C���X�g�[�����Ɋ܂܂��T���v���̈��

# == ���s�ɕK�v�Ȋ�
#    * Ruby 1.8
#    * gfdnavi_data
#      http://www.gfd-dennou.org/arch/davis/gfdnavi/download/
#      ���擾�ł���B
#      $ ruby install.rb
#      �Ƃ��ăC���X�g�[������B


# == �͂��߂�
require "numru/gfdnavi_data"
include NumRu

$KCODE = "u"

# === �w���v���o�͂ł���悤��
if ARGV.include?("-h") || ARGV.include?("--help")
  print "Usage: ruby #$0 [gfdnavi_webservice_portal_URL]\n"
  exit
end

# === URL �̏���
url_prefix = ARGV.shift || "http://0.0.0.0:3000/data"
url_prefix = url_prefix.sub(/\/\z/, "")


# == ������ǂ݁A���e�̈ꕔ��W���o�͂���
#    (Gfdnavi ���C���X�g�[�������Ƃ��A�\�ߊ܂܂�Ă��镶���̈��\������)
p "Read a sample document.\n"

#    �m�������̃t�@�C�����J��
#    open ���\�b�h�̕Ԃ�l�� GfdnaviData::KnowledgeRemote �I�u�W�F�N�g�B
k = GfdnaviData.open(url_prefix + "/gfdnavi_docs/how_to_knowledge.knlge")

#    to_knlge ���\�b�h��p���āAHash �I�u�W�F�N�g�ւƕϊ�����
k.to_knlge

#    �^�C�g���A�J�e�S���[�A���Җ��A�p�X��W���o�͂���
print "TITLE:    "
p k.title
print "CATEGOLY: "
p k.category
print "CREATOR:  "
p k.creator
print "PATH:     "
p k.path
print "\n"


#    �ȏ�B
