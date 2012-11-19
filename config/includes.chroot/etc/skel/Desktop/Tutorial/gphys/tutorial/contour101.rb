require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')

#< DCL�Υ����ץ������ >
DCL.gropn(1)
DCL.sldiv('y',2,2)           # 2x2�˲���ʬ��, 'y'=yoko: ���墪���墪����...
DCL.sgpset('lcntl', false)   # ����ʸ�����ᤷ�ʤ�
DCL.sgpset('lfull',true)     # ������ɽ��
DCL.uzfact(0.75)             # ��ɸ����ʸ���󥵥����� 0.75 ��
DCL.sgpset('lfprop',true)    # �ץ�ݡ�����ʥ�ե���Ȥ�Ȥ�

#< GGraph �ˤ�� ���� >
GGraph.set_fig('viewport'=>[0.15,0.75,0.15,0.6])    # set_*: ���ä�ͭ��������
# 1����
GGraph.contour( gphys )
GGraph.tone( gphys, false, 'ltone'=>false )        # ����ʲ��˥������ǥ���
# 2����
GGraph.next_fig('itr'=>2 )                         # next_*: ������ͭ��������
GGraph.contour( gphys.average(0), true, 'color'=>true )        # �������󥿡�
# 3����
GGraph.set_axes('xunits'=>'','yunits'=>'')  # ��ʸ���� --> ����ñ�̤Ͻ񤫤ʤ�
GGraph.tone( gphys.cut(true,true,70) )        # tone �Υǥե���Ȥϥ��顼ɽ��
GGraph.contour( gphys.cut(true,true,70), false )  # ��2���� false -> �Ťͽ�
# 4����
GGraph.set_linear_contour_options( 'min'=>0, 'nlev'=>20 )  
                 # contour�Υǥե�����ѹ�. ����Τ�ͭ����next_linear..�⤢��
GGraph.contour( gphys.average(0) )

#< �����ޤ� >
DCL.grcls
