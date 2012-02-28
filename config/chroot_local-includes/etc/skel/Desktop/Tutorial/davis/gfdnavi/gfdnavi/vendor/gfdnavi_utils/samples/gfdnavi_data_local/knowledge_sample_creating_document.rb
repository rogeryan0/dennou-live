# -*- coding: cp932 -*-
user1 = User.find(:first, :conditions => ["login=?", "root"])

# * まずは new する
new_knowledge = GfdnaviData.new("/usr/root/knowledge/creating_test/test01.knlge", user1.login, "knlge")

# * 中身を代入する
#   name, other_mode, other_readable, groups_readable,
#   default_layout, horizontal_figures, 
#   figures_size_units, figures_size_height_or_width, 
#   figures_size_number は
#   指定しなければ自動設定される
new_knowledge.title = "Temperature data from ncep."
new_knowledge.textbody = "The most heated area in Figure 1 is a point of 130 degrees of east longitude, 20 degrees of south latitude.\nThat is, it's Australia."
new_knowledge.category = "memo"
new_knowledge.creator = "Davis Taro"
new_knowledge.description = "Australia is hot."
# * 図の挿入は insert_figures メソッドを用いる。
#   画像のパス、キャプションをハッシュにし、作成者のログイン名と共に配列にして渡す。
#   複数の画像を一度に挿入することもできる。
# * Gfdnavi内で作成した画像と、既存の画像を図として使用する。
h_img1 = {"image"=>"/usr/root/_test_image01.png", "caption"=>"01\nfirst figure."}
h_img2 = {"image"=>"/samples/reanalysis/ncep/T.jan.100hPa.png", "caption"=>"02\nsecond figure!"}
new_knowledge.insert_figures = h_img1, h_img2, user1.login

# * 画像の並べ方を指定する
#   0ならheight,1ならwidthを表す
new_knowledge.figures_size_height_or_width = 0
#   0なら元の画像に対する大きさ(%)、1ならピクセルを表す
new_knowledge.figures_size_units = 1 
new_knowledge.figures_size_number = 80

# * save することにより、はじめてDBに保存される(.knlgeファイルもディスク内に作成される)
new_knowledge.save


# * 編集するときは、まず open する。
#   その後、中身を代入することで既存のものと入れ替わる。
#   save することで全てがDBとディスクに反映される。
knowledge = GfdnaviData.open("/usr/root/knowledge/creating_test/test01.knlge")
knowledge.title = "Title is overwritten."
knowledge.textbody = "teisei simasita yo.\n"
knowledge.insert_figures = {"image"=>@img_path1, "caption"=>"03."}, {"image"=>@img_path1, "caption"=>"04"}, {"image"=>@img_path1, "caption"=>"05"}, @user2.login
knowledge.save_as("/usr/root/knowledge/creating_test/test02.knlge", "root")
