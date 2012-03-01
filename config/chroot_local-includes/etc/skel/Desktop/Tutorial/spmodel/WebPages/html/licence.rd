=begin JA
= 使用上の注意
=end JA

=begin EN
= License
=end EN

=begin

# * 2012/02/29 (佐々木洋平) 更新
# * 2011/12/06 (佐々木洋平) 更新
# * 2010/04/06 (佐々木洋平) 更新
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

== Copyright and License

SPMODEL は自由に使用・改変・再配布してかまいません. ただし著作権はスペクトルモデル開発グループにあります.
詳細は((<正式なライセンス規定|URL:../spml/COPYRIGHT>))を御一読ください.

ただし内部で用いている ((<ISPACK|URL:/library/ispack/>)),
((<gtool5|URL:/library/gtool/gtool5.htm>)),
((<netCDF|URL:http://www.unidata.ucar.edu/packages/netcdf/>)),
などの他のライブラリはライセンス規定が異なるのでそれぞれ確認してください.

ライブラリ・サンプルプログラム・解説文など,
SPMODEL の資源を利用する際には,
全ての面にわたって「教育的」であることに注意してください.
ライブラリやサンプルプログラムの動作はあくまでも「無保証」です.
使用する際には各自その結果の正しさを確認してから用いてください.
解説文などにも間違いが見受けられるやもしれません.

もろもろの誤りに気づかれた際には,
下記の地球流体電脳倶楽部スペクトルモデルプロジェクトグループまで御一報頂ければ幸いです.

=== ((<引用について>))

本プログラムはフリーソフトウェアとしてリリースされていますが,
SPMODEL のライブラリを利用したり, サンプルプログラムあるいはプログラミングのノウハウ等を参考にして得られた成果を論文や web 等にて発表する際には,
その論文等にて SPMODEL を利用あるいは参考にした旨を記し, レファレンスにあげて頂きますようお願いします.

引用例は以下の通りです:

: 引用例 (和文)
  竹広 真一, 佐々木 洋平, 森川 靖大, 石岡 圭一, 小高 正嗣, 高橋 芳幸, 西澤 誠也,
  中島 健介, 石渡 正樹, 林 祥介, spmodel 開発グループ, 2011:
  階層的地球流体力学スペクトルモデル集 (SPMODEL),
  http://www.gfd-dennou.org/library/spmodel/, 地球流体電脳倶楽部.


: 引用例 (英文)
  Takehiro, S., SASAKI, Y., Morikawa, Y., Ishioka, K., Odaka, M., Takahashi, Y.O., Nishizawa, S.,
  Nakajima, K., Ishiwatari, M., Hayashi, Y.-Y.,
  SPMODEL Development Group, 2011:
  Hierarchical Spectral Models for GFD (SPMODEL),
  http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

査読付き論文しか引用できない場合には,
流体力学会オンラインジャーナル「ながれマルチメディア」の解説論文をレファレンスとしてあげてください.

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

== Copyright and License

All the resources of SPMODEL are released as Free Software, copyrighted by SPMODEL Development Group.
Please read ((<COPYRIGHT|URL:../spml/COPYRIGHT>)) in detail.

Use of SPMODEL is permitted under the principle of "NO WARRANTY" and "NO RESPONSIBILITY".
SPMODEL Development Group shall not be liable for any event arising in any way out of the use of these resources.
Redistribution in source and binary forms, with or without modification,
is also permitted provided that the above copyright notice,
disclaimer and this condition are retained.

=== ((<Citation>))

Please be aware that the fact that these programs are released as Free
Software does not excuse you from scientific propriety, which obligates
you to give appropriate credit.

If you write a scientific paper describing research that made
substantive use of these programs, it is your obligation as a scientist
to (a) mention the fashion in which this software was used with a
citation to the literature or (b) mention this software in the Acknowledgements section.

:Example
  Takehiro, S., SASAKI, Y., Morikawa, Y., Ishioka, K., Odaka, M., Takahashi, Y.O., Nishizawa, S.,
  Nakajima, K., Ishiwatari, M., Hayashi, Y.-Y.,
  SPMODEL Development Group, 2011:
  Hierarchical Spectral Models for GFD (SPMODEL),
  http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

When you have to refer only an article of a refereed journal, please
cite our ariticle of online journal of Japan Society of Fluid Mechanics
as follows:

:Example
  Takehiro, S., Odaka, M., Ishioka, K., Ishiwatari, M., Hayashi, Y.-Y., 2006:
  SPMODEL: A Series of Hierarchical Spectral Models for Geophyiscal Fluid Dynamics,
  Nagare Multimedia 2006,
  ((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))

=end

