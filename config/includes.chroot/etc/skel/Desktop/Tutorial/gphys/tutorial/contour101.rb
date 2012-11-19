require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')

#< DCLのオープンと設定 >
DCL.gropn(1)
DCL.sldiv('y',2,2)           # 2x2に画面分割, 'y'=yoko: 左上→右上→左下...
DCL.sgpset('lcntl', false)   # 制御文字を解釈しない
DCL.sgpset('lfull',true)     # 全画面表示
DCL.uzfact(0.75)             # 座標軸の文字列サイズを 0.75 倍
DCL.sgpset('lfprop',true)    # プロポーショナルフォントを使う

#< GGraph による 描画 >
GGraph.set_fig('viewport'=>[0.15,0.75,0.15,0.6])    # set_*: ずっと有効な設定
# 1枚目
GGraph.contour( gphys )
GGraph.tone( gphys, false, 'ltone'=>false )        # ゼロ以下にシェーディング
# 2枚目
GGraph.next_fig('itr'=>2 )                         # next_*: 次だけ有効な設定
GGraph.contour( gphys.average(0), true, 'color'=>true )        # 虹色コンター
# 3枚目
GGraph.set_axes('xunits'=>'','yunits'=>'')  # 空文字列 --> 軸の単位は書かない
GGraph.tone( gphys.cut(true,true,70) )        # tone のデフォルトはカラー表示
GGraph.contour( gphys.cut(true,true,70), false )  # 第2引数 false -> 重ね書き
# 4枚目
GGraph.set_linear_contour_options( 'min'=>0, 'nlev'=>20 )  
                 # contourのデフォルト変更. 次回のみ有効なnext_linear..もある
GGraph.contour( gphys.average(0) )

#< おしまい >
DCL.grcls
