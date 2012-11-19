###########################
## 2012-02-29 16:28:32 +0900
gp = gpopen 'air.2012-01.nc/air' 
tone gp
gp = gpopen "air.2012-01.nc/airrr"  # air と間違えて airrr と打ってしまった ^^;
line(gp)
mark(gp)
tone(gp)
contour(gp)
tone_and_contour(gp)
tone(gp)
tone gp
set_fig 'itr'=>10             # 正距円筒図法
set_map 'coast_world'=>true   # 地球の海岸線を表示
tone gp
set_fig 'itr'=>10             # 正距円筒図法
set_fig('itr'=>30)            # 正射図法
tone(gp)
DCL.sgscmn(3)   # 3 番のカラーマップ(黒-青-白のグラデーション)を使用
DCL.gropn(1)    # 新しい描画窓の表示 (DCL 5.4.4以降は不要になるはず)
tone(gp)
history
history_save
