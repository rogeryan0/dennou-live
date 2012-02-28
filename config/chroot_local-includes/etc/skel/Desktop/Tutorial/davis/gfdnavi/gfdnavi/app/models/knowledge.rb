# -*- coding: japanese-cp932 -*-
#require "numru/gfdnavi_data"
#require "numru/gfdnavi_data/local"

class Knowledge < NodeEntityAbstract
  has_many :knowledge_figures, :dependent => :destroy
  validates_length_of :textbody, :within => 0..1000000
  validates_format_of :category, :with => /^[a-zA-Z0-9_]+$/, :message => "you can only use alphabets and numbers and underscore to Category, and Category must not be empty."

  #before_save :before_save
  after_save :write_relations_to_db, :save_knlge_file

  attr_accessor :presaved_node_relations, :ignore_save_knlge, :ignore_check_path

  def initialize
    super
    @presaved_node_relations = Array.new
    @ignore_save_knlge = nil
    @ignore_check_path = nil
  end

  # == 知見文書の保存時、この変数を見て図を挿入する
  @@to_saved_figures = Array.new # GfdnaviDataオブジェクトの配列
  @@to_saved_figures_caption = Array.new

  # == 文書の情報を取得し、Hashにして返す
  #    (.knlgeファイルに中身を入れるときと、
  #    save_asでGfdnaviData.newするときに使っている)
  #    ...ここでGfdnaviData.newしてもいいの？よくない気が。
  def get_contents
    if (comment_on = self.comment_on)
      node = Node.find(:first, :conditions => ["id=?", comment_on], :user => self.owner)
      comment_on_path = node.path
    else
      comment_on_path = nil
    end
    knowledge_hash = {
      "title" => self.title,
      "creator" => self.creator,
      "category" => self.category,
      "textbody" => self.textbody,
      "description" => self.description,
      "default_layout" => self.default_layout,
      "horizontal_figures"=> self.horizontal_figures,
      "knowledge_figures" => [],
      "owner" => self.owner.login,
      "figures_size_height_or_width" => self.figures_size_height_or_width,
      "figures_size_units" => self.figures_size_units,
      "figures_size_number" => self.figures_size_number,
      "comment_on" => comment_on_path,
      "comment_number" => self.comment_number,
      # "rgroups" => groups,
      "other_mode" => self.other_mode,
      "other_readable" => self.other_readable,
      "groups_readable" => self.groups_readable,
      "name" => self.name
    }
    return knowledge_hash
  end

  # == 文書を作成するためのスクリプトを返す
  #    基本的にはget_contentsを利用して整形。
  #    ownerのところだけはスクリプト利用者のことを考えて、
  #    これを実行したユーザの名前にする。
  def get_script(user)
    # knowledge テーブル
    k_hash = self.get_contents
    pp k_hash
    k_str = ""
    k_str += "knowledge.url=url_prefix+\"#{self.path}\"\n"
    k_hash.each do |key, value|
      case key
      when "groups_readable","other_readable","other_mode","default_layout","figures_size_height_or_width","horizontal_figures","figures_size_number","figures_size_units"
        k_str += "knowledge.#{key}=#{value}\n"
      when "owner" # ユーザ向けに owner ではなく user という名前にしているので
        k_str += "knowledge.user=\"#{user.login}\"\n"
      when "name","knowledge_figures","comment_number","comment_on"
        next
      else
        k_str += "knowledge.#{key}=\"#{value}\"\n"
      end
    end

    # knowledge_figures テーブル
    self.knowledge_figures.each do |kf|
      if (image = Image.find(:first, :conditions => ["path=?", kf.image_path], :user => user)) && image.org_path
        k_str += "knowledge.add_figure(\"#{image.org_path}\",\"#{kf.caption}\")\n"
      else
        k_str += "knowledge.add_figure(\"#{kf.image_path}\",\"#{kf.caption}\")\n"
      end
    end

    return k_str
  end

  # == 文書に含まれる図の、元になった変数を取得する
  def relational_variables(user_name)
    user = User.find_by_login(user_name)
    nrs = NodeRelation.find(:all, :conditions => ["referenced_by=?", self.node_id])
    nodes = Array.new
    nrs.each do |nr|
      nr_node = Node.find(nr.reference)
      nodes.push(nr.reference) if nr_node.node_type == 1
    end
    return nodes
  end

  # == 文書に含まれる図の、画像を取得する
  def relational_images(user_name)
    user = User.find_by_login(user_name)
    nrs = NodeRelation.find(:all, :conditions => ["referenced_by=?", self.node_id])
    nodes = Array.new
    nrs.each do |nr|
      nr_node = Node.find(nr.reference)
      nodes.push(nr.reference) if nr_node.node_type == 2
    end
    return nodes
  end

  # == self.knowledge_figures の中身を設定する(CUI)
  #    * 引数は画像の path と caption を YAML化したもので
  #      path1, path2, ... , pathN, caption1, caption2, ... , captionN
  #      となることを想定している。
  #    * なお、knowledge_remote.rb では add_figure メソッドを用いて1枚ずつ追加する仕様になっている。
  #      (そこで追加されたものが、最終的にまとめられてこの figures= メソッドの引数になる)
  def figures=(args)
    kfs = Array.new    
    path_and_captions = YAML.load(args)
    l = path_and_captions.length / 2
    paths    = path_and_captions[0..l-1]
    captions = path_and_captions[l..-1]

    figures = Array.new
    paths.each_with_index do |path, index|
#      print "==================\nfigures.\n"
#      print "path = "
#      pp path
#      print "self.owner = "
#      pp self.owner

      # * 既存の画像を使う場合
      if image = Image.find(:first, :conditions => ["path=?", path], :user => self.owner)
        image_path = path
        kf = KnowledgeFigure.new
        kf.caption = NKF.nkf('-w', captions[index]) # UTF-8へ文字コード変換
        kf.image_path = image_path
        kf.knowledge = self # knowledgesテーブルと id で関連付ける
        kfs.push(kf)
      else
        #path = "/" + $1  if (path =~ /\/data\/(.*)$/) # http://...../data の部分を削る
        # * 今保存する場合
        ary_local = NumRu::GfdnaviData.open(path, self.owner) # GfdnaviData::ArrayLocal
        unless ary_local.is_a?(NumRu::GfdnaviData::Array)
          ary_local = NumRu::GfdnaviData::Array[ary_local]
        end
        ary_local.each do |img_local|
          # GfdnaviData::ImageLocal を image_save に渡し、画像ファイルを保存(DBとディスクの両方)
          image_path = self.image_save(self.path, index, "", img_local, self.owner)
          kf = KnowledgeFigure.new
          kf.caption = NKF.nkf('-w', captions[index]) # UTF-8へ文字コード変換
          kf.image_path = image_path
          kf.knowledge = self # knowledgesテーブルと id で関連付ける
          kfs.push(kf)
        end
      end
    end

    self.knowledge_figures = kfs
  end

=begin
    # URLからGfdnaviDataオブジェクトを生成し、@@to_saved_figures に代入
    paths.each do |path|
      #path = "/" + $1  if (path =~ /\/data\/(.*)$/) # http://...../data の部分を削る
      image = NumRu::GfdnaviData.open(path) # GfdnaviData::ArrayLocal
      image.each do |img|
        @@to_saved_figures.push img     # GfdnaviData::ImageLocal
      end
    end
    @@to_saved_figures_caption += captions
  end
=end

  # == 文書中に含まれる図の情報を取得し、HashのArrayにして返す
  def figures
    fig_infos = Array.new
    self.knowledge_figures.each do |kf|
      fig_infos.push({"image" => kf.image_path, "caption" => kf.caption})
    end
    return fig_infos # これはそのまま insert_figures= メソッドに渡すことができる
  end
  
  # == 文書に図を挿入する
  def insert_figures=(args)
    # args は GfdnaviData, String (path), もしくはその配列 + ユーザ名
    unless !args || args.length < 1 || user_name = args[-1]
      raise "Please input user name correctly.\n"
    end
    user = User.find(:first, :conditions => ["login=?", user_name])

    figs = args[0..-2]
    figs.each do |i|
      @@to_saved_figures.push(i["image"])
      @@to_saved_figures_caption.push(i["caption"])
    end
  end

  # == (既に保存された文書に対して)文書から図を削除して保存する
  #    1番目の絵は1で指定する(0ではない)
  def delete_figure(arg, user_name)
    unless self.node_id
      raise "This method can be used for saved documents.\n"
    else
      figs = self.figures
      unless figs.delete_at(arg-1) # argが範囲外だったらnilが返る
        raise "Inputed number is out of range.\n
             Please input number within the range from 1 to #{figs.length}.\n"
      end
      @@to_saved_figures = []
      @@to_saved_figures_caption = []
      figs.push(user_name)
      self.insert_figures = figs
      save("delete_figure")
    end
  end

  # == (既に保存された文書に対して)図の順番を入れ替えて保存する
  #    1番目の絵は1で指定する(0ではない)
  def swap_figures(first, second, user_name)
    unless self.node_id
      raise "This method can be used for saved documents.\n"
    else
      figs = self.figures
      if (first > figs.length) || (second > figs.length) || (first < 1) || (second < 1)
        raise "Inputed number is out of range.\n
             Please input number within the range from 1 to #{figs.length}.\n"
      end
      figs[first-1], figs[second-1] = figs[second-1], figs[first-1]
      @@to_saved_figures = []
      @@to_saved_figures_caption = []
      figs.push(user_name)
      self.insert_figures = figs
      save("swap")
    end
  end

  # 文書を閲覧する権限のあるグループを Array で取得する
  # これは node.rb に作った。(node_entity_abstract.rbから移譲)
  # def visibility
  #   Group.find_by_bit_flag(self.rgroups).collect{|g| g.name}
  # end
  
  # コメントを取得し、Arrayにして返す
  def comments(user_name)
    user = User.find_by_login(user_name)
    comments = Knowledge.find(:all, :conditions=>["comment_on=?", self.node_id], :user => user)
  end

  ##### そのリストを見て、一つのコメントを選んでさらにそれにコメントできるといい。 ####

  # self に対してのコメントを作成する。
  def make_new_comment(user_name)
    user = (login=user_name) && User.find_by_login(login)
    
    # ここでコメント番号を決める
    knowledge, knowledge_figures, comments, image_paths, image_widths, image_heights, caption_widths, caption_heights = Knowledge.read_parameters_of_knowledge_document(self.path, user)
    new_comment_number = (comments.collect{|comment| comment.comment_number}.max || 0) + 1

    # ここでコメントのパスを決める(絶対パスで表現する)
    comment_path = knowledge.path[0..-7] + "_comment_" + new_comment_number.to_s + ".knlge"

    # コメントを new して return する
    nc = NumRu::GfdnaviData.new(comment_path, user.login, "knlge")
    nc.comment_on = self.node_id # コメント元になったKnowledgeのnode_idを格納
    nc.comment_number = new_comment_number # 何番目のコメントか
    nc.title = "Re[#{new_comment_number}]:"+ knowledge.title
    nc.category = "comment"
    return nc
  end

  # == 知見文書の削除
  # * コメントを消すときには元文書のパスを引数として渡す
  # * destroyだとDBから消すメソッドと名前が被るのでこれにした
  def delete(user_name)
    kb_filenames = Array.new

    Knowledge.transaction do
      user = User.find_by_login(user_name)

      # === Knowledge(コメント)を消去
      comments = Knowledge.find(:all, :conditions => ["comment_on=?", self.node_id])
      comments.each do |comment|
        File.delete(comment.fname)
        comment.destroy
      end

      # === NodeRelation の消去
      node_relations = NodeRelation.find(:all, :conditions => ["name=? AND referenced_by=?", "based_on", self.node_id])
      node_relations.each {|nr| nr.destroy }

      # === KnowledgeBackup, KnowledgeFigureBackup の消去
      knowledge_backups = KnowledgeBackup.find(:all, :conditions => ["backup_on=?", self.path])
      knowledge_backups.each do |kb|
        kb_fname = File.dirname(self.fname)+"/"+File.basename(self.fname, ".knlge")+"."+kb.version_number.to_s+".knlge"
        kb_filenames.push kb_fname
        kb.destroy
      end

      # === Knowledge (本体)をデータベースから消去
      self.destroy
    end

    # ディスクから .knlge ファイルを消去
    File.delete(self.fname) if File.exist?(self.fname)
    # ディスクから .knlge ファイル(バックアップ)を消去
    kb_filenames.each do |fname|
      File.delete(fname) if File.exist?(fname)
    end

    # ディスクから .png ファイルとそれを格納しているディレクトリを消去(存在するならば)
    sub_dirname = File.dirname(self.fname) + "/." + File.basename(self.fname, ".knlge")
    if File.exist?(sub_dirname)
      # ディレクトリ内のファイルを消去
      Dir::glob(sub_dirname + "/*.png*").each do |png|
        File.delete(png)
      end
      # ディレクトリ自体を消去
      Dir.rmdir(sub_dirname)
    end
  end

  # == .knlgeファイルから知見文書を復旧する(DBに書き込む)
  def restore(path, k_hash, size)
    # * Knowledge(DB登録)
    Knowledge.transaction do
      self.read_knowledge_hash(path, k_hash, size=nil)
      self.save! # Knowledge.save (write to DB)
    end
    # * KnowledgeFigure(DB登録), NodeRelations(DB登録) は別途 db/register_directory_tree.rb で。
    #   (Knowledge, Image, Node  etc... が保存された後でまとめてやっている)
    # * .knlgeファイルへの書き込みは行わない
  end

  # == 知見文書をDBに保存する(ファイル(.knlge)への書き出しは別メソッド)
  #    (GUI, ディレクトリスキャン)
  def read_knowledge_hash(path, k_hash, size=nil)
    # === knowledge テーブルの中身を設定
    k_hash.each do |key, value|
      # この他、textbodyとかはKeywordAttributeにも入れないといけなかったはず。
      unless value
        # print "skip: #{key} because #{value} is nil.\n"
      else
        case key
        when "name", "path", "mtime", "size" # 何もしない
        when 'owner','user'                  # 'user' is for backward compatibility
          if (user = User.find_by_login(value))
            self.owner = user
          end
        when 'comment_on'
          # なにもしない
        else
          eval("self.#{key} = value") # そのままの型で解釈
        end
      end
    end

    if comment_path = k_hash["comment_on"]
      parent_knowledge = Knowledge.find(:first, :conditions => ["path=?", comment_path], :user => self.owner)
      self.comment_on = parent_knowledge.node_id
    end

    # === k_hash に入っていない要素と、
    #     nodeテーブルの要素(の一部)はここで入れる
=begin
    unless self.ignore_check_path
      # GUIの場合はここに来るまでにエラーになってredirect
      # CUIの場合はここでraiseする
      
      # path の文字列チェック
      error = check_path(path)
      if error
        raise error
      end

      # 同じpathが無いかチェック
      if Knowledge.find(:first, :conditions => ["path=?", self.path], :user => :all)
        raise "Failure to save!!! Inputted path \"#{self.path}\" already exists.\n"
      end
    end
=end
    self.path = path
    self.size = size
    self.mtime = Time.new
    self.name = self.title
    # knowledge の parent を探す、無ければ作る
=begin
    parent = Directory.find(:first, :conditions => ["path=?",File.dirname(self.path)])
    unless parent
      parent = Directory.new
      parent.path = File.dirname(self.path)
      parent.name = File.basename(self.path)
      parent.owner = self.owner
      parent.save!
    end
=end
    if self.owner
      FileUtils.makedirs(File.join(RAILS_ROOT, File.dirname(self.path)))
      parent = self.search_parent(self.path, self.owner)
    else
      raise "user name not exists!\n"
    end
    self.parent = parent.node
  end

  # == self.knowledge_figures の中身を設定する(GUI)
  def read_knowledge_figure_hash(kf_hash_ary)
    knlge_figs = Array.new
    kf_hash_ary.each do |kf_hash|
      kf = KnowledgeFigure.new
      kf.caption = NKF.nkf('-w', kf_hash["caption"]) # UTF-8へ文字コード変換
      kf.image_path = kf_hash["image_path"]
      kf.knowledge = self # knowledgesテーブルと id で関連付ける
      knlge_figs.push(kf)
    end
    self.knowledge_figures = knlge_figs
  end

  # == 保存前のチェック
  #    * path
  #    * input_default_value
  #    * UTF-8へ文字コード変換
  def before_save
    # path check(GUIからの保存の場合、上書きと一時保存のときはチェックを回避する)
    # 本当はチェック回避すべきではないだろう (後日要議論)
    # 新規作成でなく、更新やコメントのときに、そもそも path が変更されることは禁止すべきかもしれない
    # by S. Nishizawa (2012/02/21)
    unless self.ignore_check_path
      case self.path
      when /\.\.\//
        raise "Path Error! You input #{self.path}. \"../\" cannot be used in path."
      when /\/\.knlge/
        raise "Path Error! You input #{self.path}. Empty .knlge file name is forbidden."
      when /^\s*$/
        raise "Path Error! You input #{self.path}. Path cannot be empty."
      when /\_comment\_/
        raise "Path Error! You input #{self.path}. \"_comment_\" is forbidden string." unless self.comment_on
      when /\.\d+\.knlge$/ # .数字.knlge
        raise "Path Error! You input #{self.path}. Path like \"*****.number.knlge\" is forbidden."
      when /\/\s*.knlge$/ # /.knlge
        raise "Path Error! You input #{self.path}. Directory that doesn't have name is forbidden."
      when /\/\s*\// # // (ActiveRecordがここの判定に関係なくはじいてくれるみたい)
        raise "Path Error! You input #{self.path}. Directory that doesn't have name is forbidden."
      else
        if Knowledge.find(:first, :conditions => ["path=?", self.path], :user => :all)
          raise "Failure to save!!! Inputted path \"#{self.path}\" already exists.\n"
        end
      end
    end

    # input_default_value
    self.other_readable = 1   unless self.other_readable # true
    self.groups_readable = -1 unless self.groups_readable
    self.horizontal_figures = 1 unless horizontal_figures
    self.figures_size_height_or_width = 0 unless figures_size_height_or_width
    self.figures_size_units = 0 unless figures_size_units
    self.figures_size_number = 100 unless figures_size_number
    self.default_layout = 0 unless default_layout
    self.textbody    = "" unless self.textbody
    self.title       = "" unless self.title
    self.creator     = "" unless self.creator
    self.description = "" unless self.description
    
    # UTF-8へ文字コード変換
    ["textbody", "title", "creator", "description"].each do |column|
      eval("self.#{column} = NKF.nkf('-w', self.#{column})")
    end
  end

  # === node_relations テーブルの中身を入れる
  #     (Knowledge の node の id が必要なので、Knowledge の save 後に使うこと)
  #     (1) 引数の Hash を利用して Knowledge <-> Knowledge(コメント)の関係をDB登録。
  #     (2) self.knowledge_figures から Knowledge - Image, Knowledge - Variable の関係をDB登録。
  #     * (2)の情報もHashにし、(1)のHashに加えて return (.knlgeファイル書き込み時に利用)
  def write_relations_to_db
#    print "[write_relations_to_db]\n"
    # (1)
    self.presaved_node_relations.each do |refer_hash|
      nr = NodeRelation.new
      nr.name = refer_hash["name"]
      ref_node = Node.find(:first, :conditions => ["path=?", refer_hash["path"]], :user => self.owner)
      nr.reference = ref_node
      nr.referenced_by = self.node
      nr.save!
    end

    # (2)
    added_hash_ary = Array.new
    self.knowledge_figures.each do |kf|
      unless image = Image.find(:first, :conditions => ["path=?", kf.image_path], :user => self.owner)
        raise "Image file #{kf.image_path} is not found, so relation between Document and Image is not saved.\n"
      end
      
      # * Image <-> Knowledge
      nr = NodeRelation.new
      nr.attributes = {:name => "based_on", :reference => image.node, :referenced_by => self.node}
      nr.save!
      added_hash_ary.push({"name"=>"based_on", "path"=>image.path})

      # * Variable <-> Knowledge
      var_node_ids = Array.new
      #   Image <-> Variable の関連が既に node_relations テーブルにあるなら、そこから取ってくる。
      var_nrs = NodeRelation.find(:all, :conditions => ["referenced_by=? AND name='draw'", image.node])

      if var_nrs.length > 0
        var_nrs.each do |nr|
          var_node_ids.push nr.reference
        end
      elsif (image.org_path && vd = NumRu::GfdnaviData.open(image.org_path, self.owner).get_object)
        # image.org_path が無ければ画像はVariableと無関係。
        # vd が nil なら画像はVariableと無関係。
        vars = vd.virtual_nodes
        vars.uniq!
        vars.each do |var|
          unless var_node = Node.find(:first, :conditions => ["path=?", var.path], :user => self.owner)
            raise "File #{var.path} is not found, so relation between Document and Variable is not saved.\n"
          end
          var_node_ids.push var_node
        end
      end
      var_node_ids.uniq!

      var_node_ids.each do |var_node|
        nr = NodeRelation.new
        nr.attributes = {:reference => var_node, :referenced_by => self.node, :name => "based_on"}
        nr.save!
        added_hash_ary.push({"name"=>"based_on", "path"=>var_node.path})
      end
    end

    self.presaved_node_relations += added_hash_ary.uniq
  end

  # == 知見文書をファイル(.knlge)へ書き出す(DBへの保存は別メソッド)
  #    与えられた引数に従い、画像も同時に保存する
  def save_knlge_file
#    print "[save_knlge_file]\n"

    unless self.ignore_save_knlge
      knowledge_hash = Hash.new
      knowledge_hash = {"gfdnavi_knowledge" => self.get_contents} # knowledges テーブル
      # knowledge_figures テーブル
      self.knowledge_figures.each do |kf|
        knowledge_hash["gfdnavi_knowledge"]["knowledge_figures"].push({"caption"=>kf.caption, "image_path"=>kf.image_path})
      end
      knowledge_hash["gfdnavi_knowledge"]["reference"] = self.presaved_node_relations # node_relations テーブル
      File.open(self.fname,"w"){|file| file.print knowledge_hash.to_yaml}
    end

#    print "[end of save_knlge_file]\n"
  end


  # "self から参照されている" という情報を消す
  def delete_node_relations
    nrs = NodeRelations.find(:first, :conditions => ["referenced_by=?", self.node], :user => self.owner)
    nrs.each {|nr| nr.delete}
  end


  # == 知見文書のバックアップを作成する。
  # * knowledges テーブルのバックアップを knowledge_backups テーブルへ
  #   knowledge_figures テーブルのバックアップを knowledge_figure_backups テーブルへ。
  # * .knlgeファイルをバージョン番号つきで残す
  # 
  # * params[commit]が "Save" か "Temporary Save" のときに呼ばれる。
  # * バックアップは2種類ある。
  #   * 一時保存 : フォームの "Temporary Save" ボタンを押したときに作られる。
  #              フォームに入力された値のバックアップを保存する。.knlgeファイルは無し。
  #   * 変更履歴 : 編集機能で、上書き保存 ("Save") されたときに作られる。
  #              .knlgeファイルも作成され、中身がそこに書かれる。
  # * destroy_backup は controller に記述。GUIから直接呼ぶ。
  def create_backup(type, kf_hash_ary, groups)
    # === バックアップのバージョン番号を決める
    if type == "temporary_save"
      new_version_number = 0
    else # type == "edit_save"
      backups = KnowledgeBackup.find(:all, :conditions => ["backup_on=?", self.path])
      if backups.length == 0
        new_version_number = 1
      else
        backups.collect! {|backup| backup = backup.version_number}
        new_version_number = backups.max + 1
      end
    end
    
    # === knowledge_backups Table.
    knowledge_backup = KnowledgeBackup.copy_from_knowledge(self, type, self.owner, groups, new_version_number)
    
    # === knowledge_figure_backups Table.
    knlge_figs = Array.new
    kf_hash_ary.each do |kf_hash|
      kf = KnowledgeFigureBackup.new
      kf.caption = NKF.nkf('-w', kf_hash["caption"]) # UTF-8へ文字コード変換
      kf.image_path = kf_hash["image_path"]
      kf.knowledge_backup = knowledge_backup # knowledge_backupsテーブルと id で関連付ける
      knlge_figs.push(kf)
    end
    knowledge_backup.knowledge_figure_backups = knlge_figs

    # === save
    knowledge_backup.save
    
    # === バージョン番号をつけてバックアップファイル(.knlge)を保存する
    if type == "edit" # 上書き
      # * knowledge_backups Table.
      knowledge_backup_hash = {"gfdnavi_knowledge" => self.get_contents}

      # knowledge_figure_backups テーブル
      knowledge_backup.knowledge_figure_backups.each do |kf|
        knowledge_backup_hash["gfdnavi_knowledge"]["knowledge_figures"].push({"caption"=>kf.caption, "image_path"=>kf.image_path})
      end
      File.open(self.fname[0..-7] + "." + new_version_number.to_s + ".knlge","w"){|file| file.print knowledge_backup_hash.to_yaml}
    end
  end
  
=begin
  # == 知見文書の保存
#  def save(type=nil)
  def save_hogehoge(type=nil)
    # type のバリエーション:
    #   new, new_from_analysis, temporary_save,
    #   swap, delete_figure, edit_save, edit_save_as, directory_scan, comment

    Knowledge.transaction do
      KnowledgeFigure.transaction do 
        #    ActiveRecord::Base::transaction do
        check_path(self.path) unless self.comment_on || !type || (type == "comment") || (type == "edit_save") || (type == "edit_save_as")

        # 既に同じpathをもつ文書がある場合、そこから図を取ってくる (コマンドラインからのedit時)
        # コマンドラインからのedit時には、save の引数 edit は指定しないのでこれで大丈夫
        unless type=="swap" || type=="delete_figure" || type=="edit_save" || type=="directory_scan"
          if (k = Knowledge.find(:first, :conditions => ["path=?", path]))
            k.figures.reverse.each do |fig|
              @@to_saved_figures.unshift(fig["image"])
              @@to_saved_figures_caption.unshift(fig["caption"])
            end
          end
        end

        # === knowledge の parent を決める
        FileUtils.makedirs(File.join(RAILS_ROOT, File.dirname(self.path)))
        parent = Directory.find(:first, :conditions => ["path=?",File.dirname(self.path)])
        unless parent
          parent = Directory.new
          parent.path = File.dirname(self.path)
          parent.name = File.basename(self.path)
          parent.owner = self.owner
          parent.save!
        end

        # === self.path を使って、図となる画像を格納するディレクトリの名前を決める.
        fig_dir_path = File.join(parent.path, "." + File.basename(self.path, ".knlge"))

        # ここからはテーブルの中身の設定
        # (ディレクトリスキャン時は既に中身が入っているので不要)
        unless type=="directory_scan"
          # === knowledge_figures テーブルの中身を設定
          knlge_figs = Array.new

          # 上書き保存のとき、既存の画像は消去し、新たに入れ直す
          # (元あった画像のコピーは既に @@to_saved_figures_caption に入っている)
          if type == "edit_save"
            kfs = KnowledgeFigure.find(:all, :conditions => ["knowledge_id = ?", self.id])
            kas = keyword_attributes = KeywordAttribute.find(:all, :conditions => ["node_id = ?", self.node])
            kfs.each {|kf| kf.destroy }
            kas.each {|ka| ka.destroy }
          end

          @@to_saved_figures.each_with_index do |figure, i|
            #  ==== 画像のpath (image_path)を取得する
            if String === figure
              # * 既存の画像を用いる場合は figure がそのまま path
              image_path = figure
            else
              # figure は Stringじゃなかったら GfdnaviDataのはず
              # if NumRu::GfdnaviData === figure
              # * 画像を文書と共に保存する場合, path をここで決める
              if i < 10
                prefix = "image00"
              elsif i < 100
                prefix = "image0"
              else
                prefix = "image"
              end
              image_path = File.join(fig_dir_path, prefix + i.to_s + ".png")

              # 作成者がスーパーユーザじゃない場合、
              # save_as メソッド(lib/virtual_data.rb) に渡すために /usr/ユーザ名 を取り除く
              unless self.owner.super_user?
                /^\/usr\/(.+?)(\/.+)$/ =~ image_path
                for_db_path = $2
              else
                for_db_path = image_path
              end

              figure.save_as(for_db_path, self.owner)
            end

            knlge_fig = KnowledgeFigure.new
            knlge_fig.knowledge = self # knowledgesテーブルと id で関連付ける
            knlge_fig.image_path = image_path
            knlge_fig.caption = @@to_saved_figures_caption[i]
            knlge_figs.push(knlge_fig)
          end

          # === knowledges テーブルの中身を設定
          self.knowledge_figures = knlge_figs # knowledge_figuresテーブルと id で関連付ける
          self.horizontal_figures = 1 unless horizontal_figures
          self.figures_size_height_or_width = 0 unless figures_size_height_or_width
          self.figures_size_units = 0 unless figures_size_units
          self.figures_size_number = 100 unless figures_size_number
          self.default_layout = 0 unless default_layout
        end # unless type=="directory_scan"

        # === node テーブルの中身を設定
        self.name = self.title
        self.other_readable = 1 # true
        self.groups_readable = -1
        self.parent = parent.node
        self.mtime = Time.new

        # === 知見文書の文字コードを UTF-8 に変換する
        #     (本文、表題、著者、要約、図のキャプション)
        self.textbody    = NKF.nkf('-w', self.textbody)
        self.title       = NKF.nkf('-w', self.title)
        self.creator     = NKF.nkf('-w', self.creator)
        self.description = NKF.nkf('-w', self.description)
        self.knowledge_figures.each do |fig|
          fig.caption    = NKF.nkf('-w', fig.caption)
        end

        # ここまでテーブルへの中身の代入

        # === save とその後の処理
#        unless super # Knowledge.save
        unless self.save
          # raise "failed to save a document (in knowledge.rb): #{self.errors.inspect}"
          return false
        else
          # ==== パラメータ初期化
          @@to_saved_figures = []
          @@to_saved_figures_caption = []

          # ==== node_relations table
          # + knowledgeのnode_idが必要なので、これは知見文書の保存後に行う必要がある。
          # + 記録する関係
          #   * reference: 数値データ, referenced_by: 知見文書, name: "based_on"
          #   * reference: 画像, referenced_by: 知見文書, name: "based_on"
          # + Save(上書き)のときのみ、前の node_relations を消す
          if type == "edit_save"
            self.delete_node_relations
          end

          # + node_relations テーブルに関係を記録する
          # ++ KnowledgeFigureを使って、画像と変数の配列をそれぞれ取得
          variables = Array.new
          images = Array.new
          self.knowledge_figures.each do |kf|
            if (img = Image.find(:first, :conditions=>["path=?", kf.image_path], :user => @user))
              images << img # 画像の配列
              # Gfdnavi内で生成された画像ならばここがtrueになる(外部の画像はorg_pathを持たない)
              if img.org_path
                vd = NumRu::GfdnaviData.open(img.org_path).get_object
                variables += vd.virtual_nodes # 画像の基になった変数の配列
              end
            end
          end

          # ++ 上で取得した変数と画像を node_relationsテーブルに入れる
          node_relations = Array.new
          (images + variables).uniq.each do |var|
            node_relation = NodeRelation.new
            node_relation.attributes = {:reference => var.node, :referenced_by => self.node, :name => "based_on"}
            node_relations << node_relation
            node_relation.save
          end

          unless type=="directory_scan"
            # ==== .knlge ファイルの作成(ディレクトリスキャン時は上書きしない)
            # * knowledge テーブル
            knowledge_hash = {"gfdnavi_knowledge" => self.get_contents}
            # * knowledge_figures テーブル
            self.knowledge_figures.each do |kf|
              knowledge_hash["gfdnavi_knowledge"]["knowledge_figures"].push({"image_path" => kf.image_path, "caption"  => kf.caption})
            end
            # * node_relations テーブル
            node_relation_hash = {"reference" => []}
            (images + variables).uniq.each do |var|
              node_relation_hash["reference"].push({"name" => "based_on", "path" => var.path})
            end
            knowledge_hash["gfdnavi_knowledge"].merge!(node_relation_hash) if node_relation_hash

            # + .knlgeファイルへの書き込み
            File.open(self.fname,"w"){|file| file.print knowledge_hash.to_yaml}
          end # end of unless type=="directory_scan"
          return true
        end # end of unless super
      end # end of KnowledgeFigure.transaction
    end # end of Knowledge.transaction
    #    end # end of transaction
  end
=end

  # 知見文書の別名保存
  def save_as(path, user_name)
    knowledge_hash = self.get_contents
    knowledge_hash.delete("knowledge_figures")
    knowledge_hash.delete("owner")
    new_gd = NumRu::GfdnaviData.new(path, user_name, "knlge", knowledge_hash)
    new_gd.insert_figures = self.figures.push(user_name)
    new_gd.save("edit_save_as")
  end

  # == self に関わる NodeRelation を消す
  def delete_node_relations
    old_node_relations = NodeRelation.find(:all, :conditions => ["name=? AND referenced_by=?", "based_on", self.node_id])
    old_node_relations.each do |relation|
      relation.destroy
    end
  end

  # == (知見文書のひとつ下のディレクトリに)Image を保存する
  #    保存された Image オブジェクトを返す
  def image_save(path, index, name, img_local, user)

    # ==== 保存する画像のパスを決める
    # + path からディレクトリを決める
=begin
    parent = Directory.find(:first, :conditions => ["path=?",File.dirname(path)])

    unless parent
      parent = Directory.new
      parent.path = File.dirname(path)
      parent.name = File.basename(path)
      parent.owner = user
      parent.save!
    end
=end
    FileUtils.makedirs(File.join(RAILS_ROOT, File.dirname(path)))
    parent = self.search_parent(path, user)
    img_dirname = File.join(parent.path, "." + File.basename(path, ".knlge"))

    # + name からファイル名を決める
    unless name == ""
      img_name = name+".png"
    else
      prefix = "image"
      if index < 10
        prefix += "00"
      elsif 10 <= index && index < 100
        prefix += "0"
      end
      img_name = prefix + index.to_s + ".png"
    end
    # + くっつけてパスを作る
    img_path = File.join(img_dirname, img_name)

    # ==== 画像を保存する(DBにもディスクにも)
    # 作成者がスーパーユーザじゃない場合、
    # save_as (lib/virtual_data.rb内) に渡すために /usr/ユーザ名 を取り除く
    unless user.super_user?
      /^\/usr\/(.+?)(\/.+)$/ =~ img_path
      db_path = $2
    else
      db_path = img_path
    end
    img_local.save_as(db_path, user)

    unless image_obj = Image.find(:first, :conditions => ["path=?", img_path], :user => self.owner)
      raise "image (#{db_path}) not found.\n"
    end
    
    return image_obj.path
  end

  # == parent directory を探す。
  # / から順に全て調べ、無ければ作る。(DBのみ、実体はこの中では保存しない)
  # ループした後、最後のディレクトリを parent として return.
  def search_parent(path, user)
    full_path = ""
    parent = nil

    path.split(File::Separator)[0..-2].each do |dname|
      full_path = File.join(full_path, dname)
      dir = Directory.find(:first, :conditions=>["path=?",full_path], :user=>user)
      unless dir
        dir = Directory.new
        dir.name = dname
        dir.path = full_path
        dir.parent = parent.node
        dir.owner = user
        dir.other_mode = self.other_mode
        dir.rgroups = self.rgroups
        if full_path == self.file
          dir.downloadable = self.downloadable?
          dir.plain_file = true
        end
        dir.save!
      end
      parent = dir
    end
    return parent
  end

  #<< class methods >>
  class << self
    def read_parameters_of_knowledge_document(path, user)
      knowledge = Knowledge.find(:first, :conditions => ["path=?", path], :user => user)
      comments = Knowledge.find(:all, :conditions => ["comment_on = ?" ,knowledge.node_id], :order => "comment_number", :user=>user)
      knowledge_figures = KnowledgeFigure.find(:all, :conditions => ["knowledge_id=?", knowledge.id]) # not found -> return []

      # = 絵のパスと大きさ
      #   * 絵のパスをnodesテーブルのpathより取得
      #   * 絵の大きさをimagesテーブルのvizshotより取得
      image_paths = Array.new
      image_widths = Array.new
      image_heights = Array.new
      caption_widths = Array.new
      caption_heights = Array.new
      knowledge_figures.each do |kf|
        unless (image = Node.find(:first, :conditions => ["path = ?", kf.image_path], :user => user))
          #raise "Figure is not found.\n"

          # vizshotが無いときはnormalサイズを適用する
          image_widths << 250
          image_heights << 254
          caption_widths << 440
          caption_heights << 440
        else
          image_paths << image.path
=begin
          vizshot_yaml = image.org_path
          begin
            vizshot = YAML.load(vizshot_yaml)
            if (image_size = vizshot.get_size)
              image_widths << image_size[0]
              image_heights << image_size[1]
              caption_widths << (image_size[0] * 0.8).to_i
              caption_heights << (image_size[1] * 0.8).to_i
            else
              # vizshotから復元できなかったときはnormalサイズを適用する
              image_widths << 554
              image_heights << 554
              caption_widths << 440
              caption_heights << 440
            end
          rescue
=end
            # vizshotが無いときはnormalサイズを適用する
            image_widths << 250
            image_heights << 254
            caption_widths << 440
            caption_heights << 440
#          end
        end
      end
      return knowledge, knowledge_figures, comments, image_paths, image_widths, image_heights, caption_widths, caption_heights
    end
    
    # OpenIDの接頭につく http:// や https:// 、また末尾につく / を取り除く
    def remove_scheme(user)
      if user.internal_user
        user.login
      else
        username_in_path = user.login
        case username_in_path
        when /^http:\/\/(.+)/, /^https:\/\/(.+)/
          username_in_path = $1
        end
        
        case username_in_path
        when /(.+)\/$/
          username_in_path = $1
        end
        return username_in_path
      end
    end
  end
end



