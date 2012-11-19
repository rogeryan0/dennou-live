# -*- coding: japanese-cp932 -*-
# = knowledge_sample_reading_document.rb
#   Gfdnavi の Webサービスを用いて知見文書を読み取り、
#   種々の情報を標準出力に表示するサンプルプログラム
#   * ログインの必要は無い
#   * 読み取るのは、Gfdnaviインストール時に含まれるサンプルの一つ

# == 実行に必要な環境
#    * Ruby 1.8
#    * gfdnavi_data
#      http://www.gfd-dennou.org/arch/davis/gfdnavi/download/
#      より取得できる。
#      $ ruby install.rb
#      としてインストールする。


# == はじめに
require "numru/gfdnavi_data"
include NumRu

$KCODE = "u"

# === ヘルプを出力できるように
if ARGV.include?("-h") || ARGV.include?("--help")
  print "Usage: ruby #$0 [gfdnavi_webservice_portal_URL]\n"
  exit
end

# === URL の処理
url_prefix = ARGV.shift || "http://0.0.0.0:3000/data"
url_prefix = url_prefix.sub(/\/\z/, "")


# == 文書を読み、内容の一部を標準出力する
#    (Gfdnavi をインストールしたとき、予め含まれている文書の一つを表示する)
p "Read a sample document.\n"

#    知見文書のファイルを開く
#    open メソッドの返り値は GfdnaviData::KnowledgeRemote オブジェクト。
k = GfdnaviData.open(url_prefix + "/gfdnavi_docs/how_to_knowledge.knlge")

#    to_knlge メソッドを用いて、Hash オブジェクトへと変換する
k.to_knlge

#    タイトル、カテゴリー、著者名、パスを標準出力する
print "TITLE:    "
p k.title
print "CATEGOLY: "
p k.category
print "CREATOR:  "
p k.creator
print "PATH:     "
p k.path
print "\n"


#    以上。
