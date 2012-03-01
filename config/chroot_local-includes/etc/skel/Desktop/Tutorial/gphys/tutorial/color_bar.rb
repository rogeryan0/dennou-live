require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')

#< DCL�̃I�[�v���Ɛݒ� >
DCL.gropn(1)
DCL.sldiv('y',2,2)           # 2x2�ɉ�ʕ���, 'y'=yoko: ���と�E�と����...
DCL.sgpset('isub', 96)       # ���t���������䕶���ύX: '_' --> '`'
DCL.sgpset('lfull',true)     # �S��ʕ\��
DCL.uzfact(0.75)             # ���W���̕�����T�C�Y�� 0.75 �{
DCL.sgpset('lfprop',true)    # �v���|�[�V���i���t�H���g���g��

#< GGraph �ɂ�� �`�� >
GGraph.set_fig('viewport'=>[0.15,0.7,0.2,0.6])    # set_*: �����ƗL���Ȑݒ�
# 1����
GGraph.tone( gphys.cut(true,true,850) )
GGraph.contour( gphys.cut(true,true,850), false )
GGraph.color_bar                          # �J���[�o�[ (�f�t�H���g�͏c)

# 2����
GGraph.next_fig('viewport'=>[0.2,0.75,0.2,0.6])
GGraph.tone( gphys.cut(true,true,850) )
GGraph.contour( gphys.cut(true,true,850), false )
GGraph.color_bar('left'=>true,'labelintv'=>1)   # �����ɕ\��, �S�����x��(��1����)

# 3����
GGraph.next_fig('itr'=>2 )
GGraph.tone( gphys.mean(0), true, 'int'=>4)
GGraph.color_bar('vlength'=>0.5,"landscape"=>true,'tickintv'=>0)
                    # �����w��, ���ɕ\��, tick mark�Ȃ�(0�̏ꍇ�̓���)

# 4����
GGraph.next_fig('itr'=>2 )
rmiss = DCL.glpget('rmiss')
GGraph.tone( gphys.mean(0), true, 'levels'=>
             [rmiss,-70,-60,-50,-40,-30,-20,-10,0,10,20,rmiss] )
GGraph.color_bar("voff"=>0.04)        # ������Ɨ]���ɉ��ɗ���
                                      # �}������� GrADS ���ɎO�p�ɂȂ�

#< �����܂� >
DCL.grcls
