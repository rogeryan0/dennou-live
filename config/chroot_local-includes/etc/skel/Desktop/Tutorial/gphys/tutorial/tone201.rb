require "numru/ggraph"
include NumRu
gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')

#< DCL�Υ����ץ������ >
DCL.gropn(1)
DCL.sldiv('y',2,2)           # 2x2�˲���ʬ��, 'y'=yoko: ���墪���墪����...
DCL.sgpset('lcntl', false)   # ����ʸ�����ᤷ�ʤ�
DCL.sgpset('lfull',true)     # ������ɽ��
DCL.sgpset('lfprop',true)    # �ץ�ݡ�����ʥ�ե���Ȥ�Ȥ�

#< GGraph �ˤ�� ���� >
GGraph.set_fig('viewport'=>[0.15,0.82,0.15,0.6])
# 1����
GGraph.set_tone_levels( 'levels'=>[-20,-15,-10,-5,0],
                        'patterns'=>[10999,20999,30999,40999] )
GGraph.tone( gphys )
# 2����
GGraph.tone( gphys, true, 'lev'=>[-20,0,20],     # ��٥���ѥ�������ۤ˻���
             'pat'=>[20999,40999,70999,80999] ) # �ѥ��������1��¿���ޡ�ޤ�
GGraph.contour( gphys, false, 'lev'=>[-20,0,20], 'index'=>3 )      # ���ͤޤ�
# 3����
GGraph.tone( gphys, true, 'lev'=>[-20,0,20], 
                    'pat'=>[40999,70999,80999] ) # ��٥��Ʊ�����ܡ�ޤǳ�ĥ
# 4����
GGraph.tone( gphys, true, 'lev'=>[-20,0,20], 
                    'pat'=>[40999,70999] )    # �ѥ���Τۤ���1�ľ����֤��ɤ�
#< �����ޤ� >
DCL.grcls
