=begin JA
= 使用上の注意
=end JA

=begin EN
= Licence
=end EN

=begin

# * 2009/01/12 (竹広真一) spmodel 内リンクを相対パスに変更
# * 2007/12/05 (佐々木洋平) Makefile.rd2html の更新に合わせた修正
# * 2007/07/19 (竹広真一) 最終更新 
# * 2007/04/24 (小高正嗣) 最終更新 
# * 2005/09/30 (竹広真一) 最終更新 
# * 2005/07/21 (小高正嗣) 最終更新 
# * 2005/06/30 (小高正嗣) 最終更新 
# * 2005/04/01 (小高正嗣) 最終更新 
# * 2005/04/01 (小高正嗣)
# * 2005/03/30 (小高正嗣)
# * 2004/03/18 (小高正嗣)
# * 2004/01/26 (小高正嗣)
# * 2002/09/12 (竹広真一, 林祥介)
# * 2002/02/15 (竹広真一) 新規作成

=end

=begin JA

SPMODEL は研究・教育の場で用いられることを前提としております.
地球流体電脳倶楽部(SPMODEL 開発グループ)の活動を妨げるような利用の仕方
でない限り, 自由に使用・改変・再配布してかまいません. 
((<正式なライセンス規定|URL:../spml/COPYRIGHT>))を御一読ください. 
ただし内部で用いている ((<ISPACK|URL:/library/ispack/>)),
((<gt4f90io|URL:/library/gtool4/>)),
((<netCDF|URL:http://www.unidata.ucar.edu/packages/netcdf/>)),
などの他のライブラリはライセンス規定が異なるのでそれぞれ確認してください.

ライブラリ・サンプルプログラム・解説文など, SPMODEL の資源を利用する際
には, 全ての面にわたって「教育的」であることに注意してください.  ライ
ブラリやサンプルプログラムの動作はあくまでも「無保証」です.  使用する
際には各自その結果の正しさを確認してから用いてください.  解説文などに
も間違いが見受けられるやもしれません.

もろもろの誤りに気づかれた際には, 下記の地球流体電脳倶楽部スペクトルモ
デルプロジェクトグループまで御一報頂ければありがたいです.

=== 引用例

SPMODEL のライブラリを利用したり, サンプルプログラムあるいはプログラミ
ングのノウハウ等を参考にして得られた成果を論文や web 等にて発表する際
には, できるだけその論文等にて SPMODEL を利用あるいは参考にした旨を記
し, レファレンスにあげて頂きますようお願いします.

: 引用例 (和文)

    竹広真一, 石岡圭一, 西澤誠也, 谷口博, 森川靖大, 小高正嗣, 石渡正樹, 林祥介, 
    SPMODEL 開発グループ, 2007:  
    階層的地球流体力学スペクトルモデル集 (SPMODEL),  
    http://www.gfd-dennou.org/library/spmodel/, 地球流体電脳倶楽部. 

#    竹広真一, 石岡圭一, 西澤誠也, 谷口博, 森川靖大, 小高正嗣, 石渡正樹, 林祥介, 
#    SPMODEL 開発グループ, 2006:  
#    階層的地球流体力学スペクトルモデル集 (SPMODEL),  
#    http://www.gfd-dennou.org/library/spmodel/, 地球流体電脳倶楽部. 

: 引用例 (英文)

    Takehiro, S., Ishioka, K., Nishizawa, S., Taniguchi, H., Morikawa, Y., Odaka, M., Ishiwatari, M., Hayashi, Y.-Y.,
    SPMODEL Development Group, 2007: 
    Hierarchical Spectral Models for GFD (SPMODEL),
    http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

#    Takehiro, S., Ishioka, K., Kakinami, Y., Nishizawa, S., Morikawa, Y., Odaka, M., Ishiwatari, M., Hayashi, Y.-Y.,
#    SPMODEL Development Group, 2005: 
#    Hierarchical Spectral Models for GFD (SPMODEL),
#    http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

査読付き論文しか引用できない場合には, 流体力学会オンラインジャーナル「なが
れマルチメディア」の解説論文をレファレンスとしてあげてください. 

: 査読つき論文の引用例 (和文)

  竹広真一, 小高正嗣, 石岡圭一, 石渡正樹, 林祥介, SPMODEL 開発グループ, 2006:
  階層的地球流体スペクトルモデル集 SPMODEL, 
  ながれマルチメディア 2006, 
  ((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))

: 査読つき論文の引用例 (英文)

  Takehiro, S., Odaka, M., Ishioka, K., Ishiwatari, M., Hayashi, Y.-Y., 2006:
  SPMODEL: A Series of Hierarchical Spectral Models for Geophyiscal Fluid Dynamics,
  Nagare Multimedia 2006, 
  ((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))

=end 
=begin EN

Please read ((<COPYRIGHT|URL:../spml/COPYRIGHT>)) file.

=== Citation

Takehiro, S., Ishioka, K., Nishizawa, S., Taniguchi, H., Morikawa, Y., Odaka, M., Ishiwatari, M., Hayashi, Y.-Y.,
SPMODEL Development Group, 2007: 
Hierarchical Spectral Models for GFD (SPMODEL),
http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

#    Takehiro, S., Ishioka, K., Kakinami, Y., Nishizawa, S., Morikawa, Y., Odaka, M., Ishiwatari, M., Hayashi, Y.-Y.,
#    SPMODEL Development Group, 2007: 
#    Hierarchical Spectral Models for GFD (SPMODEL),
#    http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

When you have to refer only an article of a refereed journal, please 
cite our ariticle of online journal of Japan Society of Fluid Mechanics
as follows:

  Takehiro, S., Odaka, M., Ishioka, K., Ishiwatari, M., Hayashi, Y.-Y., 2006:
  SPMODEL: A Series of Hierarchical Spectral Models for Geophyiscal Fluid Dynamics,
  Nagare Multimedia 2006, 
  ((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))

=end

