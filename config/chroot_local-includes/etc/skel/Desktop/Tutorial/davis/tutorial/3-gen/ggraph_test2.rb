require "numru/ggraph"
include NumRu
file = NetCDF.open("uwnd.mon.ltm.nc")
uwnd = GPhys::NetCDF_IO.open(file,"uwnd")
DCL.uzfact(0.6)

gobjs = [
   GGraph.open(1) ,        # ���͊e�`�施�߂̓I�u�W�F�N�g������Ċe���\�b�h��
   GGraph.contour(uwnd.mean(0)) ,    # �߂�l�ƂȂ��Ă���B�����Ȃ�z��Ƃ���
   GGraph.contour(uwnd[true,true,5,0])  ,
   GGraph.close  ]

gobjs.flatten!                 # ����q�ɂȂ��Ă�z����t���b�g��
gobjs.each{|i| p i.class}      # �e�R�}���h�̃N���X��\�����Ă݂�
gobjs.each{|i| i.exec}         # �Ď��s --- ��قǕ`�������̂��Č������
