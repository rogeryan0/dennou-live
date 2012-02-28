# -*- coding: cp932 -*-
user1 = User.find(:first, :conditions => ["login=?", "root"])

# * �܂��� new ����
new_knowledge = GfdnaviData.new("/usr/root/knowledge/creating_test/test01.knlge", user1.login, "knlge")

# * ���g��������
#   name, other_mode, other_readable, groups_readable,
#   default_layout, horizontal_figures, 
#   figures_size_units, figures_size_height_or_width, 
#   figures_size_number ��
#   �w�肵�Ȃ���Ύ����ݒ肳���
new_knowledge.title = "Temperature data from ncep."
new_knowledge.textbody = "The most heated area in Figure 1 is a point of 130 degrees of east longitude, 20 degrees of south latitude.\nThat is, it's Australia."
new_knowledge.category = "memo"
new_knowledge.creator = "Davis Taro"
new_knowledge.description = "Australia is hot."
# * �}�̑}���� insert_figures ���\�b�h��p����B
#   �摜�̃p�X�A�L���v�V�������n�b�V���ɂ��A�쐬�҂̃��O�C�����Ƌ��ɔz��ɂ��ēn���B
#   �����̉摜����x�ɑ}�����邱�Ƃ��ł���B
# * Gfdnavi���ō쐬�����摜�ƁA�����̉摜��}�Ƃ��Ďg�p����B
h_img1 = {"image"=>"/usr/root/_test_image01.png", "caption"=>"01\nfirst figure."}
h_img2 = {"image"=>"/samples/reanalysis/ncep/T.jan.100hPa.png", "caption"=>"02\nsecond figure!"}
new_knowledge.insert_figures = h_img1, h_img2, user1.login

# * �摜�̕��ו����w�肷��
#   0�Ȃ�height,1�Ȃ�width��\��
new_knowledge.figures_size_height_or_width = 0
#   0�Ȃ猳�̉摜�ɑ΂���傫��(%)�A1�Ȃ�s�N�Z����\��
new_knowledge.figures_size_units = 1 
new_knowledge.figures_size_number = 80

# * save ���邱�Ƃɂ��A�͂��߂�DB�ɕۑ������(.knlge�t�@�C�����f�B�X�N���ɍ쐬�����)
new_knowledge.save


# * �ҏW����Ƃ��́A�܂� open ����B
#   ���̌�A���g�������邱�ƂŊ����̂��̂Ɠ���ւ��B
#   save ���邱�ƂőS�Ă�DB�ƃf�B�X�N�ɔ��f�����B
knowledge = GfdnaviData.open("/usr/root/knowledge/creating_test/test01.knlge")
knowledge.title = "Title is overwritten."
knowledge.textbody = "teisei simasita yo.\n"
knowledge.insert_figures = {"image"=>@img_path1, "caption"=>"03."}, {"image"=>@img_path1, "caption"=>"04"}, {"image"=>@img_path1, "caption"=>"05"}, @user2.login
knowledge.save_as("/usr/root/knowledge/creating_test/test02.knlge", "root")
