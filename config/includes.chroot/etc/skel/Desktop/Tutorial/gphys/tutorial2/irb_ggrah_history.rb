###########################
## 2012-02-29 16:28:32 +0900
gp = gpopen 'air.2012-01.nc/air' 
tone gp
gp = gpopen "air.2012-01.nc/airrr"  # air �ƊԈႦ�� airrr �Ƒł��Ă��܂��� ^^;
line(gp)
mark(gp)
tone(gp)
contour(gp)
tone_and_contour(gp)
tone(gp)
tone gp
set_fig 'itr'=>10             # �����~���}�@
set_map 'coast_world'=>true   # �n���̊C�ݐ���\��
tone gp
set_fig 'itr'=>10             # �����~���}�@
set_fig('itr'=>30)            # ���ː}�@
tone(gp)
DCL.sgscmn(3)   # 3 �Ԃ̃J���[�}�b�v(��-��-���̃O���f�[�V����)���g�p
DCL.gropn(1)    # �V�����`�摋�̕\�� (DCL 5.4.4�ȍ~�͕s�v�ɂȂ�͂�)
tone(gp)
history
history_save
