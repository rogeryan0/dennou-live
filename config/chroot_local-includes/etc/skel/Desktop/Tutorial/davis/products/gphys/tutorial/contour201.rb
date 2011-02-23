require "numru/ggraph"
include NumRu
gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')

#< DCLのオープンと設定 >
DCL.gropn(1)
DCL.sldiv('y',2,2)           # 2x2に画面分割, 'y'=yoko: 左上→右上→左下...
DCL.sgpset('lcntl', false)   # 制御文字を解釈しない
DCL.sgpset('lfull',true)     # 全画面表示
DCL.sgpset('lfprop',true)    # プロポーショナルフォントを使う

#< GGraph による 描画 >
GGraph.set_fig('viewport'=>[0.15,0.82,0.15,0.6])
levels = 5*( NArray.float(7).indgen! )                # NArray: [0,5,10,...]
mj = DCL.udpget('indxmj')
mn = DCL.udpget('indxmn')
# 1枚目
GGraph.set_contour_levels( 'levels'=>levels, 'index'=>mj )    # コンター設定
GGraph.contour( gphys )
# 2枚目
GGraph.contour( gphys, true, 'lev'=>levels, 
                'index'=>[mj,mn], 'line_type'=>[1,2,2,2] )
# 3枚目
GGraph.contour( gphys, true, 'lev'=>levels, 
                'index'=>mn, 'label'=>true )
# 4枚目
GGraph.contour( gphys, true, 'lev'=>levels, 
                'index'=>mn, 'label'=>['A','B','C','D'],
                'label_height'=>[0.015,0.02,0.025,0.03] )
#< おしまい >
DCL.grcls
