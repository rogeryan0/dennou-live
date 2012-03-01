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
# 1枚目
GGraph.set_tone_levels( 'levels'=>[-20,-15,-10,-5,0],
                        'patterns'=>[10999,20999,30999,40999] )
GGraph.tone( gphys )
# 2枚目
GGraph.tone( gphys, true, 'lev'=>[-20,0,20],     # レベル＆パターンを陽に指定
             'pat'=>[20999,40999,70999,80999] ) # パタンの方が1つ多→±∞まで
GGraph.contour( gphys, false, 'lev'=>[-20,0,20], 'index'=>3 )      # 参考まで
# 3枚目
GGraph.tone( gphys, true, 'lev'=>[-20,0,20], 
                    'pat'=>[40999,70999,80999] ) # レベルと同数→＋∞まで拡張
# 4枚目
GGraph.tone( gphys, true, 'lev'=>[-20,0,20], 
                    'pat'=>[40999,70999] )    # パタンのほうが1つ少→間を塗る
#< おしまい >
DCL.grcls
