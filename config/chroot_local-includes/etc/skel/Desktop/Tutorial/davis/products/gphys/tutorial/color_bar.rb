require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')

#< DCLのオープンと設定 >
DCL.gropn(1)
DCL.sldiv('y',2,2)           # 2x2に画面分割, 'y'=yoko: 左上→右上→左下...
DCL.sgpset('isub', 96)       # 下付を示す制御文字変更: '_' --> '`'
DCL.sgpset('lfull',true)     # 全画面表示
DCL.uzfact(0.75)             # 座標軸の文字列サイズを 0.75 倍
DCL.sgpset('lfprop',true)    # プロポーショナルフォントを使う

#< GGraph による 描画 >
GGraph.set_fig('viewport'=>[0.15,0.7,0.2,0.6])    # set_*: ずっと有効な設定
# 1枚目
GGraph.tone( gphys.cut(true,true,850) )
GGraph.contour( gphys.cut(true,true,850), false )
GGraph.color_bar                          # カラーバー (デフォルトは縦)

# 2枚目
GGraph.next_fig('viewport'=>[0.2,0.75,0.2,0.6])
GGraph.tone( gphys.cut(true,true,850) )
GGraph.contour( gphys.cut(true,true,850), false )
GGraph.color_bar('left'=>true,'labelintv'=>1)   # 左側に表示, 全部ラベル(∵1個おき)

# 3枚目
GGraph.next_fig('itr'=>2 )
GGraph.tone( gphys.mean(0), true, 'int'=>4)
GGraph.color_bar('vlength'=>0.5,"landscape"=>true,'tickintv'=>0)
                    # 長さ指定, 横に表示, tick markなし(0の場合の特例)

# 4枚目
GGraph.next_fig('itr'=>2 )
rmiss = DCL.glpget('rmiss')
GGraph.tone( gphys.mean(0), true, 'levels'=>
             [rmiss,-70,-60,-50,-40,-30,-20,-10,0,10,20,rmiss] )
GGraph.color_bar("voff"=>0.04)        # ちょっと余分に横に離す
                                      # ±無限大は GrADS 風に三角になる

#< おしまい >
DCL.grcls
