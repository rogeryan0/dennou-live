# -*- coding: japanese-cp932 -*-
require "numru/gfdnavi_data/knowledge"

module NumRu::GfdnaviData
  class KnowledgeRemote < NumRu::GfdnaviData::Knowledge

    def to_knlge
      get_object("knlge")
    end

    def initialize(hash={})
      @@figure_paths    = []
      @@figure_captions = []
      super(hash)
    end

    # == KnowledgeRemote用のメソッドを定義する。
    #    app/models/knowledge.rb で定義されたメソッド名を利用する
    meths = ["category","creator","textbody","default_layout",\
             "horizontal_figures","figures_size_height_or_width", \
             "figures_size_units","figures_size_number",\
             "comment_on","comment_number"]
    meths.collect!{|me| /\A(.+)_id\z/ =~ me ? $1 : me}
    meths += ["get_contents", "relational_images", "relational_variables",\
              "figures", "insert_figures", "swap_figures", "delete_figure",\
              "comments", "make_new_comment", "save", "delete"]

    # * 上記の名前について、下記の内容でメソッドを定義する
    meths.each do |name|
        eval <<-EOL, binding, __FILE__, __LINE__+1
        def #{name}=(arg)
          get_object("knlge") unless @new_data
          @representation[\"knlge\"] ||= {\"gfdnavi_knowledge\"=>Hash.new}
          @representation[\"knlge\"][\"gfdnavi_knowledge\"][\"#{name}\"] = arg
        end
        EOL

        eval <<-EOL, binding, __FILE__, __LINE__+1
        def #{name}
          unless @representation[\"knlge\"] && @representation[\"knlge\"][\"gfdnavi_knowledge\"]
            return nil
          end
          @representation[\"knlge\"][\"gfdnavi_knowledge\"][\"#{name}\"]
        end
        EOL
    end

    # == 画像を末尾に挿入するメソッド
    #    @representation に画像の URL を YAML 化したものを持たせる
    #    (YAML化するのは文字列の配列をまとめて一つの文字列にするため)
    #    挿入は画像一枚ずつとなる。
    def add_figure(image, caption)
      # 引数を参照し、@@figure_paths と @@figure_captions の中身に追記していく
      unless caption
        caption = ""
      end

      if String === image
        @@figure_paths    << image
      else
        # @@figure_paths    << image.url
        @@figure_paths    << image.path  # http://はこの時点でつけないことにする
        # knowledgeを登録するところならpath、そうでないならurlというのが
        # 理想のはず。
      end
      @@figure_captions << caption

      # Local(サーバ)側へ文字列を送る。
      # これらは、Local側の figures= メソッドの引数となる(add_figure ではなく。)
      # Localの方では、送られた YAML ファイルを参照し、複数枚の図を一度に入れる仕様になっている。
      # Remote で add_figure が呼び出される度に @@figure_paths と @@figure_captions の中身に
      # 追記されていき、随時 Hash の中身を更新していく。
      @representation["knlge"] ||= {"gfdnavi_knowledge"=>Hash.new}
      @representation["knlge"]["gfdnavi_knowledge"]["figures"] = String.new
      @representation["knlge"]["gfdnavi_knowledge"]["figures"] = YAML.dump(@@figure_paths + @@figure_captions)
    end

    def update_save_data(hash)
      # * hash を受け取る側(Local, サーバ)は 
      #   gl.key = value という形になる。
      #   つまり、key というメソッドに、 value という引数を渡す。
      #   value は文字列のみ対応。
      hash.update(@representation["knlge"]["gfdnavi_knowledge"])
    end
  end
end
