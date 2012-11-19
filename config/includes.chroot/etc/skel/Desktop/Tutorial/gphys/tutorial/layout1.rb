require "numru/ggraph"
include NumRu
gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')

DCL.gropn(1)
DCL.sgpset('lcntl', false)   # ����ʸ�����ᤷ�ʤ�
DCL.sgpset('lfull',true)     # ������ɽ��
DCL.sgpset('lcorner',false)  # �����ʡ��ޡ�����񤫤ʤ�
DCL.uzfact(0.35)             # ��ɸ����ʸ���󥵥����������
DCL.sgpset('lfprop',true)    # �ץ�ݡ�����ʥ�ե���Ȥ�Ȥ�
DCL.udpset('lmsg',false)     # ���󥿡��ֳ���ɽ��

vpt = NArray[0.05, 0.45, 0.05, 0.25]             # �ӥ塼�ݡ��ȥ����� (2:1)
vpt00 = ( vpt + ([0.050]*2 + [0.32]*2) ).to_a    # x,y�����ˤ��餷��Array��
vpt01 = ( vpt + ([0.474]*2 + [0.32]*2) ).to_a    # x,y�����ˤ��餷��Array��
vpt10 = ( vpt + ([0.050]*2 + [0.10]*2) ).to_a    # x,y�����ˤ��餷��Array��
vpt11 = ( vpt + ([0.474]*2 + [0.10]*2) ).to_a    # x,y�����ˤ��餷��Array��

GGraph.set_fig('viewport'=>vpt00)
GGraph.set_axes('xunits'=>'','yunits'=>'','xtitle'=>'') 
DCL.uzpset('labelxb',false)
GGraph.contour( gphys.cut(true,true,1000), true, 'annot'=>false, 'titl'=>'' )
DCL.uzpset('pad1',0.2) ; DCL.uxsttl('t','1000 hPa',-1) ; DCL.uzpset('pad1',0.7)

GGraph.set_fig('viewport'=>vpt01, 'new_frame'=>false)
GGraph.set_axes('ytitle'=>'')
DCL.uzpset('labelyl',false)
GGraph.contour( gphys.cut(true,true,250), true, 'annot'=>false, 'titl'=>'' )
DCL.uzpset('pad1',0.2) ; DCL.uxsttl('t','250 hPa',-1) ; DCL.uzpset('pad1',0.7)

GGraph.set_fig('viewport'=>vpt10, 'new_frame'=>false)
GGraph.set_axes('ytitle'=>nil,'xtitle'=>nil)
DCL.uzpset('labelyl',true); DCL.uzpset('labelxb',true)
GGraph.contour( gphys.cut(true,true,70), true, 'annot'=>false, 'titl'=>'' )
DCL.uzpset('pad1',0.2) ; DCL.uxsttl('t','70 hPa',-1) ; DCL.uzpset('pad1',0.7)

GGraph.set_fig('viewport'=>vpt11, 'new_frame'=>false)
GGraph.set_axes('ytitle'=>'')
DCL.uzpset('labelyl',false)
GGraph.contour( gphys.cut(true,true,10), true, 'annot'=>false, 'titl'=>'' )
DCL.uzpset('pad1',0.2) ; DCL.uxsttl('t','10 hPa',-1) ; DCL.uzpset('pad1',0.7)

DCL::sgtxzv(0.5,vpt00[3]+0.028,'January Monthly Mean Temperature',
	    1.15*DCL.uzpget('rsizec2'),0,0,3)

DCL.grcls

print "\n** PRESSURE LEVELS ** " ; p gphys.coord(2).val
