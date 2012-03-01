require "numru/ggraph"
include NumRu
gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')

#< DCL�̃I�[�v���Ɛݒ� >
DCL.gropn(1)
DCL.sldiv('y',2,2)           # 2x2�ɉ�ʕ���, 'y'=yoko: ���と�E�と����...
DCL.sgpset('lcntl', false)   # ���䕶�������߂��Ȃ�
DCL.sgpset('lfull',true)     # �S��ʕ\��
DCL.sgpset('lfprop',true)    # �v���|�[�V���i���t�H���g���g��

#< GGraph �ɂ�� �`�� >
GGraph.set_fig('viewport'=>[0.15,0.82,0.15,0.6])
levels = 5*( NArray.float(7).indgen! )                # NArray: [0,5,10,...]
mj = DCL.udpget('indxmj')
mn = DCL.udpget('indxmn')
# 1����
GGraph.set_contour_levels( 'levels'=>levels, 'index'=>mj )    # �R���^�[�ݒ�
GGraph.contour( gphys )
# 2����
GGraph.contour( gphys, true, 'lev'=>levels, 
                'index'=>[mj,mn], 'line_type'=>[1,2,2,2] )
# 3����
GGraph.contour( gphys, true, 'lev'=>levels, 
                'index'=>mn, 'label'=>true )
# 4����
GGraph.contour( gphys, true, 'lev'=>levels, 
                'index'=>mn, 'label'=>['A','B','C','D'],
                'label_height'=>[0.015,0.02,0.025,0.03] )
#< �����܂� >
DCL.grcls
