# -*- coding: cp932 -*-
require "numru/gfdnavi_data/knowledge"
require "forwardable"

module NumRu::GfdnaviData
  class KnowledgeLocal < NumRu::GfdnaviData::Knowledge
    extend Forwardable

    OBJECT_CLASS = ::Knowledge

    # * 大半のメソッドは Knowledge オブジェクトに移譲する
    #   (models/knowledge.rb の同名のメソッドと同じ働きをするようにする)
    #
    #if Node.table_exists? && OBJECT_CLASS.table_exists?
    #meths = Node.column_names.dup + OBJECT_CLASS.column_names.dup
    if OBJECT_CLASS.table_exists?
      meths = OBJECT_CLASS.column_names.dup
      meths.delete("id")
      meths.collect!{|me| /\A(.+)_id\z/ =~ me ? $1 : me} # ...._id は _id を消す
      meths += ["get_contents", "relational_images", "relational_variables", "figures", "insert_figures", "swap_figures", "comments", "make_new_comment", "save", "delete", "delete_figure"]
      # figures= は Local のみ。Remote は add_figure を使う。
      meths.each do |me|
        def_delegator :@object, me
        def_delegator :@object, me+"="
      end
    end

    def errors
      @object.errors.full_messages
    end

    def to_knlge
      # typeのチェックが必要
      path = @object.path
      if /\A\/usr(\/.*)\Z/ =~ path
        path = GFDNAVI_USER_PATH + $1
      else
        path = GFDNAVI_DATA_PATH + path
      end
      knowledge_file = File.open(path)
      knowledge_yml = knowledge_file.read
      knowledge_hash = YAML.load(knowledge_yml)
      return knowledge_hash
    end

    def initialize(hash={})
      # hashの中身をいじるなら、ここでやる
      super(hash) # これは必須

      # 引数が {} なら新規作成
      # (パスを指定せずに単にKnowledgeオブジェクトを作成するだけ、
      # 機能としては実装は後回し)という扱いにし、パスの処理は行わない。
      unless hash=={}
        # === パスの処理
        # * Local:
        #    * スーパーユーザの場合
               #必ず絶対パスとみなす(どこにでもファイルを置ける)
        #    * 一般ユーザの場合
        #      "/usr/" + user_name + "/knowledge/" + @path の形に
        #      なっていなければ相対パスとみなす(/knowledge/以下にしかファイルを置けない)
        #    * ユーザ指定なしの場合
        #      一般ユーザに同じ。
        # * Remote: 
        #      http:// からはじまるので、パスが省略されることはない
        path = @object.path
        if @user
          unless @user.super_user
            unless /^\/usr\/(.*)\/knowledge\/.*/ =~ path
              path = "/usr/" + user_name + "/knowledge/" + path
            end
          end
        end
        @object.path = path
        @object.owner = @user
      end
    end
  end
end

