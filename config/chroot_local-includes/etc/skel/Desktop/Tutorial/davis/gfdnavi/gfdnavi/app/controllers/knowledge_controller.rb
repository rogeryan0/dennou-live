# -*- coding: japanese-cp932 -*-
require "numru/vizshot_gfdnavi"
require "virtual_data"
require "pp"

class KnowledgeController < ApplicationController
  layout "gfdnavi", :except => [:list_without_layout, :show_without_layout, :category_search, :comment_written]
  before_filter :login_required, :except => [:index, :list, :list_without_layout, :show, :show_without_layout, :appear_comment_input_form, :fig2analysis, :fig2analysis2, :category_search]
  verify :method => :post, :only => [ :destroy_document, :create, :update ], :redirect_to => { :action => :list }

  def index
    list
    render :action => 'list'
  end

  # == paginateを利用して、知見ドキュメントの一覧を表示する
  def list
    @user = (login=session[:user]) && User.find_by_login(login)
    
    # この部分のコードは仕方なく複雑な作りになっている。
    # paginate メソッドは内部的に find メソッドを使っているが
    # この find は Rails で定義されたオリジナルのものである。
    # 一方、使いたかったのは node.rb で再定義した find。
    # ということで、重複するが node.rb の find と同じような記述をした。
    # list_without_layout についても同様。
    other_readable_conditions = Node.conditions_to_read(@user)
    unless @user # 非ログイン状態
      conditions = " AND (#{other_readable_conditions})" # sqlite3, MySQLの両方に対応
    else
      if @user.super_user
        conditions = ""
      elsif @user.groups == 0 # ユーザがグループに所属しない場合
        conditions = " AND (( (owner_id = #{@user.id}) OR (#{other_readable_conditions})))"
      else                    # ユーザが何らかのグループに所属する場合
        conditions = " AND (( (owner_id = #{@user.id}) OR NOT ((groups_readable & 1) = 0) OR (#{other_readable_conditions})))"
      end
    end
    
    # ソート
    unless params['sort']
      order = "mtime DESC"
    else
      case params['sort']
      when "time"
        sort_by = "mtime"
      when "author"
        sort_by = "creator"
      else
        sort_by = params['sort']
      end
      order = sort_by
      order += " DESC" if params['reverse_sort'] == 'reverse_sort'
    end
    
    # 絞り込み
    if params["commit"] == "Search"
      conditions += " AND (knowledges.category = \"#{params['category']}\")" unless params["category"] == ""
      conditions += " AND (knowledges.creator = \"#{params['creator']}\")" unless params["creator"] == ""
    end
    
    # ページングして表示する
    @knowledges = Knowledge.paginate_by_sql("SELECT knowledges.* FROM nodes, knowledges WHERE ((knowledges.node_id=nodes.id) AND (node_type=3) #{conditions} AND (knowledges.comment_on IS NULL)) ORDER BY #{order}", :page => params[:page], :per_page => GFDNAVI_PAGINATE_PER)
  end
  
  # == 他の画面に埋め込むために、
  # 画面上部のGfdnaviのメニューを出さずにlistを表示する
  # コメントの一覧表示などに使われる。
  def list_without_layout
    other_readable_conditions = Node.conditions_to_read(@user)
    @list_without_layout = true
    @user = (login=session[:user]) && User.find_by_login(login)
    if params[:node_ids]
      node_ids = params[:node_ids]
    end
    
    unless @user # 非ログイン状態
      conditions = " AND (#{other_readable_conditions})"
    else
      if @user.super_user
        conditions = ""
      elsif @user.groups == 0 # ユーザがグループに所属しない場合
        conditions = " AND (( (owner_id = #{@user.id}) OR (#{other_readable_conditions})))"
      else                    # ユーザが何らかのグループに所属する場合
        conditions = " AND (( (owner_id = #{@user.id}) OR NOT ((groups_readable & 1) = 0) OR (#{other_readable_conditions})))"
      end
    end

    # コメントの場合は comment_number の順、そうでなければ新しい順に表示
    if params[:from] == "show_comments"
      order = "comment_number"
    else
      order = "mtime DESC"
    end

    node_ids_sql = "'"
    node_ids.each do |id|
      node_ids_sql += id.to_s + "','"
    end
    node_ids_sql = node_ids_sql[0..-3]

#    @knowledges = Knowledge.paginate_by_sql("SELECT knowledges.* FROM nodes, knowledges WHERE ((node_id IN (#{node_ids_sql})) AND (knowledges.node_id=nodes.id) AND (node_type=3) AND ((other_readable))) ORDER BY #{order}", :page => params[:page], :per_page => 5)
    # とりあえず paginate はしない。エラーになるので。
#    @knowledges = Knowledge.find_by_sql("SELECT knowledges.* FROM nodes, knowledges WHERE ((node_id IN (#{node_ids_sql})) AND (knowledges.node_id=nodes.id) AND (node_type=3) AND ((other_readable))) ORDER BY #{order}")
#    @knowledges = Knowledge.paginate(:all, {:user => @user}, {:page => params[:page], :per_page => 3, :conditions => ["(node_id IN (?))", node_ids], :order => order })
    @knowledges = Knowledge.find(:all, :conditions => ["(node_id IN (?))", node_ids], :order => order, :user => @user)
    @not_paginate = true
    render :action => 'list'
  end

  # 知見ドキュメントを閲覧する
  def show
    begin
      @user = (login=session[:user]) && User.find_by_login(login)
      @knowledge, @knowledge_figures, @comments, @image_paths, @image_widths, @image_heights, @caption_widths, @caption_heights = Knowledge.read_parameters_of_knowledge_document(params[:path], @user)
    rescue
      #unless params[:from] && params[:from] == "create"
      unless params[:from] && params[:from] == "update"
        # createした後showに飛ばされたときは以下のメッセージを表示させない
        flash[:notice] = "The knowledge document doesn't exist."
      end
      redirect_to :action => 'list'
    else
      @num_of_figure = 1 # コメント投稿欄を出現させたときに必要
    end
  end

  # 画面上部のGfdnaviのメニューを出さずにshowを実行する
  def show_without_layout
    @show_without_layout = true
    show
    render :action => 'show'
  end

  # 知見ドキュメントを作成するためのスクリプトを表示させる
  def script
    begin
      user = (login=session[:user]) && User.find_by_login(login)
      knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:path]], :user => user)
      @knowledge_script = knowledge.get_script(user)
    rescue => exception
      # 上の部分の、knowledge.save の内部では
      # 保存前に before_save メソッドが実行されている。
      # ここから上がってきた例外を捕捉する。
      flash[:notice] = exception
      redirect_to :action => 'list'
    end
  end


  # list.rhtml -> new.rhtml (Click "New Knowledge" button.)
  def new
    @type = "new"
    @knowledge = Knowledge.new
    @user = (login=session[:user]) && User.find_by_login(login)
    if @user.super_user
      @groups = Group.find(:all)
    else
      @groups = @user.belonging_groups
    end
    @num_of_figure = 1
  end

  # Click "Click a Knowledge Document with this/these Image(s)" button in Analysis
  def new_from_analysis
    @type = "new_from_analysis"
    @knowledge = Knowledge.new
    @user = (login=session[:user]) && User.find_by_login(login)
    if @user.super_user
      @groups = Group.find(:all)
    else
      @groups = @user.belonging_groups
    end
    session[:knowledge] = Hash.new
    session[:knowledge_figure] = Hash.new

    # 解析画面で描かれた絵のパスを取得する
    unless diagram_paths = session[:diagrams].dup
      @num_of_figure = 0
    else
      session[:knowledge_figure]["image_paths"] = diagram_paths
      @num_of_figure = diagram_paths.length
    end
  end

  # Edit a knowledge document.
  # list.rhtml -> edit.rhtml (Click "Edit" link.)
  def edit
#    print "== edit ==\n"
#    print "params = "
#    pp params

    @type = "edit"
    @user = (login=session[:user]) && User.find_by_login(login)
    if @user.super_user
      @groups = Group.find(:all)
    else
      @groups = @user.belonging_groups
    end
    
    # 編集対象の文書の内容をあらかじめフォームに書き込んでおく
    @knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:path]], :user => @user)
    unless @user == @knowledge.owner || @user.super_user
      flash[:notice] = "Sorry, you don't have the right to edit this document."
      redirect_to :action => "list"
    end
    session[:knowledge] = Hash.new
    session[:knowledge_figure] = Hash.new
    @figure_captions = Array.new
    @figure_paths = Array.new
    
    # 文書中の図についても同様
    knowledge_figures = @knowledge.knowledge_figures
    knowledge_figures.each do |kf|
      img = Image.find(:first, :conditions => ["path = ?", kf.image_path], :user => @user)
      @figure_captions << kf.caption
      @figure_paths << img.path
    end

    @num_of_figure = @knowledge.knowledge_figures.length
  end

  # == create and save Knowledge Document
  # 変数 @knowledge と knowledge_figures に入力フォームから取得した値を代入して
  # 最後に save (temporary save も行う).
  # 
  # save, temporary save の場合分けは
  # * 一時保存ならバックアップのみ保存
  # * 上書き保存なら本体とバックアップの両方を保存
  # * そのほかの場合は本体のみ保存
  #def create
  def update
#    require 'pp'
#    print "== update ==\n"
#    print "params = "
#    pp params

    @type = params[:type]
    @commit = params[:commit]

=begin
    case params[:type]
    when "new", "new_from_analysis", "comment", "temporary_save"
        @type = params[:type]
    when "edit"
      case params[:commit]
      when "Save"
        @type = "edit_save"
      when "Save As"
        @type = "edit_save_as"
      end
    end
=end

    user = (login=session[:user]) && User.find_by_login(login)

    # 入力内容チェック
    unless @type == "temporary_save"
      if params["knowledge"]["category"] == ""
        category_error = "Category Error! Please input \"category\".\n"
      end
    end

    # === knowledges テーブル
    # ==== Knowledge オブジェクトを用意。上書きのときだけは find してきて、その id を使う。
    if @type=="edit" && @commit=="Save" # @type == "edit_save"
      knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:original_path]], :user => user)
      knowledge.presaved_node_relations = Array.new
      knowledge_copy = knowledge.dup
    else
      knowledge = Knowledge.new
    end

    # ==== ハッシュから中身を入れていく
    k_hash = params["knowledge"]
    # owner は上書き保存時は元のものをそのまま使用する(別の人になってしまわないように)
    if @type=="edit" && @commit=="Save" # @type == "edit_save"
      k_hash["owner"] = knowledge_copy.owner
    else
      k_hash["owner"] = user.login
    end

    # コメントの場合は、親となる文書のid を取得する
    if @type=="comment"
      unless parent_knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:path]], :user => user)
        raise "error. parent knowledge document not found.\n"
      end
#      print "parent_knowledge = "
#      pp parent_knowledge
    end

    # コメント関連のパラメータを入れる(comment_on, comment_number)
    if @type == "comment"
      k_hash["comment_on"]     = parent_knowledge.path
      k_hash["comment_number"] = params["knowledge"]["comment_number"]

#      print "k_hash = "
#      pp k_hash
    end

    # 可読かどうかをグループ別に設定
    # (基本的にDB保存は models で行うが、rgroups と other_mode だけはここで代入しておく)
    # * コメントの場合、親となる文書の設定を引き継ぐ。
    if @type == "comment"
      knowledge.rgroups = parent_knowledge.rgroups
      knowledge.other_mode = parent_knowledge.other_mode
    else
      if @type == "temporary_save" # temporary なら内容についてのお叱りは受けない 
        groups = []
      else
        if params[:groups]
          groups = params[:groups].values # Hash to Array
        else
          flash[:notice] = "Check at least 1 group to visible!"
          redirect_to :action => "edit", :from_list => "from_list", :path => knowledge.path
          return
        end
      end

      if groups.length > 0
        # rgroups column
        knowledge.node.set_rgroups(groups)
        # other_mode column
        if groups[0] == 'everyone'
          other_mode = 4
          groups.shift
        else
          other_mode = 0
        end
        knowledge.other_mode = other_mode
      end
    end

    # 画像関連のパラメータを入れる
    ### 本当は view の方も名前を合わせておいて、paramsからそのままコピーするだけで済むようにすべき
    if k_hash["height_or_width"] == "width"
      k_hash["figures_size_height_or_width"] = 1
    else 
      k_hash["figures_size_height_or_width"] = 0
    end
    k_hash.delete("height_or_width")
    if k_hash["percent_or_pixel"] == "pixel"
      k_hash["figures_size_units"] = 1
    else 
      k_hash["figures_size_units"] = 0
    end

    # layoutを"figures in a row above text."にした場合はこれがnullになっているので
    unless k_hash["horizontal_figures"]
      k_hash["horizontal_figures"] = 1
    end
    k_hash.delete("percent_or_pixel")

    # === nodes テーブル
    # path を決める。上書きの時だけは既存のものを使用。
    if @type=="edit" && @commit=="Save" # @type == "edit_save"
      path = params[:original_path]
    else
      # path の dirname を決める。一般ユーザは /usr/ 以下にしかファイルを置けない。
      path = params["node"]["path"] + ".knlge"
      if user.super_user
        path = "/" + path
      else
        path = "/usr/"+ Knowledge.remove_scheme(user) + "/knowledge/" + path
      end
    end

    # 編集時の上書き保存や一時的な保存の場合、パスの重複などはチェックしないようにする。
    # (modelのbefore_saveで制御)
    if (@type=="edit" && @commit=="Save") || (@type == "temporary_save") || (@type=="comment" && @commit=="Add a Comment on this document.")
      knowledge.ignore_check_path = true
    end

    # === knowledge_figures テーブル
    #     まず図を正しい順番にソートする。sortはArrayを返すので、後でHashに戻してやる。
    kf_hash_ary = []
    if params["knowledge_figure"]
      sorted_kf = params["knowledge_figure"].sort {|a, b| a[0].to_i <=> b[0].to_i}
      sorted_kf.each {|key, value| kf_hash_ary.push value } # Hash の Array にする
      kf_hash_ary.delete_if {|kf_hash| kf_hash["figure_path"] == ""} # path が無いものは削除
    end

    notfound_error = ""
    # kf_hash に期待されているのは、image_path と caption のみ
    kf_hash_ary.each_with_index do |kf_hash, index|
      # 画像のpathをHashに格納する。
      # pathが無い = まだ保存されていない　場合はまず画像を保存する
      #
      # new_from_analysis 以外で 下の find が nil になるとしたら
      # 存在しない画像のパスを貼ったとき。

      if image = Image.find(:first, :conditions => ["path=?", kf_hash["figure_path"] + ".png"], :user => user)
        # 既存の画像の場合はそのpathを利用
        image_path = image.path
      else
        # 新規の画像とみなす(kf_hash["figure_path"]には org_path が入っている)
        if @type=="new_from_analysis"
          gd = NumRu::GfdnaviData.open(kf_hash["figure_path"], user)
          image_path = knowledge.image_save(path[0..-7], index, kf_hash["name"], gd, user)
        else
          # new_from_analysis以外の場合は新規の画像は入らない。パスが間違っている。
          notfound_error += "Image not found."
        end
      end

      # image_path が nil ならパス(kf_hash["figure_path"])が間違っているとしてエラー
      unless image_path
        notfound_error += "#{kf_hash['figure_path']}.png not found.\n"
      else
        kf_hash["image_path"] = image_path
        kf_hash["caption"] = kf_hash["figure_caption"]
        kf_hash.delete("name")
        kf_hash.delete("figure_number")
        kf_hash.delete("diagram_id")
        kf_hash.delete("figure_caption")
        kf_hash.delete("figure_path")
      end
    end

    # 画像が見つからなかった場合はエラーにする
    unless notfound_error == ""
      flash[:notice] = notfound_error
      redirect_to :action => 'list'
    else
      comment_hash = nil
      refer_hash_ary = Array.new
      # === 保存
      begin
        Knowledge.transaction do
          knowledge.delete_node_relations if @type=="edit" && @commit=="Save" # @type == "edit_save"
          knowledge.read_knowledge_hash(path, k_hash, size=nil)
          knowledge.read_knowledge_figure_hash(kf_hash_ary)

          # @knowledge, @user が render後に必要
          case @commit
          when "Save As", "Create"
            knowledge.save! # Knowledge.save
            @knowledge = knowledge
            flash[:notice] = "A knowledge document is successfully saved."
            redirect_to :action => 'show', :path => path, :from => 'update'
          when "Save"
            knowledge.save! # Knowledge.save
            knowledge_copy.create_backup(@type, kf_hash_ary, groups) # KnowledgeBackup.save
            @knowledge = knowledge
            flash[:notice] = "A knowledge document is successfully saved."
            redirect_to :action => 'show', :path => path, :from => 'update'
          when "Add a Comment on this document."
            knowledge.save! # Knowledge.save
            @knowledge = knowledge
            redirect_to :action => 'comment_written', :path => parent_knowledge.path
          when "Save Temporarily"
            knowledge.id = params[:original_id]
            knowledge.create_backup(@type, kf_hash_ary, groups) # KnowledgeBackup.save
            if params[:original_path]
              @knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:original_path]], :user => user)
            end
            @user = user
            #          flash[:notice] = "Temporary save.\n"
            render :partial => "backup"
          end
        end
      rescue => exception
        # 上の部分の、knowledge.save の内部では
        # 保存前に before_save メソッドが実行されている。
        # ここから上がってきた例外を捕捉する。
        flash[:notice] = exception
        redirect_to :action => 'list'       
      end
    end
  end

  # コメントが書かれた後にrenderするため。
  def comment_written
    @user = (login=session[:user]) && User.find_by_login(login)
    @knowledge, @knowledge_figures, @comments, @image_paths, @image_widths, @image_heights, @caption_widths, @caption_heights = Knowledge.read_parameters_of_knowledge_document(params[:path], @user)
    @num_of_figure = 1 # コメント投稿欄を出現させたときに必要
  end

=begin
    @user = (login=session[:user]) && User.find_by_login(login)
    if params[:type]
      case params[:type]
      when "new", "new_from_analysis", "comment", "temporary_save"
        @type = params[:type]
      when "edit"
        case params[:commit]
        when "Save"
          @type = "edit_save"
        when "Save As"
          @type = "edit_save_as"
        end
      end
    end
    
    # === knowledges table
    unless @type == "edit_save"
      @knowledge = Knowledge.new
    else
      # ==== Click "Save" button in edit.rhtml
      # * if "Save", insert all original data include "@Knowledge.id"
      begin
        @knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:original_path]], :user => @user)
        @old_knowledge = Knowledge.new
        @old_knowledge.id = @knowledge.id
        @old_knowledge.attributes = @knowledge.attributes
        @old_knowledge.node.attributes = @knowledge.node.attributes
        @old_knowledge.knowledge_figures = @knowledge.knowledge_figures
      rescue
        raise "Specified knowledge is not found or not yours."
      end
    end

    # * about groups
    if (!params[:groups] || @type == "comment")
      @groups = ['everyone']
    else
      # raise "Check at least 1 group to visible!\n" unless params[:groups]
      @groups = params[:groups].values # Hash to Array
    end
    
    if @groups[0] == 'everyone'
      other_mode = 4
      @groups.shift
    else
      other_mode = 0
    end
    @knowledge.other_mode = other_mode

    # * insert title, textbody(and change for RD-like format)
    #   , default_layout, horizontal number of figures
    #   , height/width, %/px, and size(number).
    horizontal_figures = params[:knowledge]["horizontal_figures"] || 1
    figures_size_height_or_width = params[:knowledge]["height_or_width"] == "width" ? 1 : 0
    figures_size_units = params[:knowledge]["percent_or_pixel"] == "pixel" ? 1 : 0
    figures_size_number = params[:knowledge]["figures_size_number"] || 100
    
    if @type == "comment"
      # for creating comment, insert knowledge.id to "comment_on" and "comment_number"
      if (knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:path]], :user => @user))
        @knowledge.attributes = {"comment_on" => knowledge.node_id}
        @knowledge.attributes = {"comment_number" => params[:knowledge]["comment_number"]}
      end
    end

    @knowledge.attributes = {
      "category" => params[:knowledge]["category"],
      "title" => params[:knowledge]["title"],
      "creator" => params[:knowledge]["creator"],
      "textbody" => params[:knowledge]["textbody"],
      "description" => params[:knowledge]["description"],
      "default_layout" => params[:knowledge]["default_layout"] || 0,
      "horizontal_figures" => horizontal_figures,
      "figures_size_height_or_width" => figures_size_height_or_width,
      "figures_size_units" => figures_size_units,
      "figures_size_number" => figures_size_number
    }
    
    # === nodes table
    # * insert path, owner, name for knowledge.node    
    #   * 上書き時には、元のパスを用いる
    #   * それ以外の時は、入力されたパスを用いる
    if @type == "edit_save"
      begin
        @knowledge.path = params[:original_path]
      rescue
        flash[:notice] = "Unknown error. The path of original knowledge document doesn't exists."
        redirect_to :action => 'list'
      end
    else
      if params[:node]["path"] && params[:node]["path"] != ""
        filename = params[:node]["path"] + ".knlge"
      elsif params[:original_path]
        filename = params[:original_path] + ".knlge"
      else
        filename = ".knlge"
      end
      
      if @user.super_user
        @knowledge.path = "/" + filename
      else
        @knowledge.path = "/usr/"+ Knowledge.remove_scheme(@user) + "/knowledge/" + filename
      end
    end

    @knowledge.name = File.basename(@knowledge.path)
    
    # 上書き保存時、たとえ元のユーザと別のユーザ(スーパーユーザなど)が書き換えたとしても、ownerは変更しない
    @knowledge.owner = @user unless params[:commit] == "Save"

    # groups    
    @knowledge.node.set_rgroups(@groups) if @groups.length > 0

    # if none, make directory.
    #   * path in the disc
    FileUtils.makedirs(File.dirname(@knowledge.fname)) or raise("failed to makedir")
    #   * path in gfdnavi
    make_directories(@knowledge, @user, @groups, other_mode)

    # === knowledge_figures table
    fig_args = Array.new
    figure_paths = Array.new
    if params[:knowledge_figure]
      params[:knowledge_figure].sort {|a, b| a[0].to_i <=> b[0].to_i}.each do |key, value| # 絵の順番を正しくソート
        unless (value["figure_path"] == "" && value["figure_caption"] == "")
          if image = Image.find(:first, :conditions => ["path=?", value["figure_path"] + ".png"], :user => @user)
            # * 既存の画像の場合 (find して、見つかれば既存の画像とみなす)
            fig_args.push({"image" => image.path, "caption" => value["figure_caption"]})
            figure_paths.push(image.path) # 後でnode_relationsテーブルを保存するときに使う
          else
            # * Gfdnavi内で作成した画像と共に文書を保存する場合
            gfdnavi_data = NumRu::GfdnaviData.open(value["figure_path"])
            fig_args.push({"image" => gfdnavi_data, "caption" => value["figure_caption"]})
          end
        end # end of unless
      end # end of each
    end # end of (if params[:knowledge_figure])
    fig_args.push(@user.login)

    @knowledge.insert_figures = fig_args

    # 検索用に、title と textbody と author(creator) と category を keyword_attributes に入れる。
    @knowledge.keyword_attributes.build(:name => "title", :value => params[:knowledge]["title"])
    @knowledge.keyword_attributes.build(:name => "textbody", :value => params[:knowledge]["textbody"])
    @knowledge.keyword_attributes.build(:name => "description", :value => params[:knowledge]["description"])
    @knowledge.keyword_attributes.build(:name => "creator", :value => params[:knowledge]["creator"])
    @knowledge.keyword_attributes.build(:name => "category", :value => params[:knowledge]["category"])

    # 更新時、mtimeを上書き
    @knowledge.mtime = Time.new unless params[:commit] == "Create"
    
    # * 一時保存ならバックアップのみ保存
    # * 上書き保存なら本体とバックアップの両方を保存
    # * そのほかの場合は本体のみ保存
    if @type == "temporary_save"
      create_backup
      @type = params[:type]
      # 上書き保存時のバックアップ生成のときは、元データのIDもテーブルに入れる
      @knowledge = Knowledge.find(params[:document_id], :user => @user) if @type == "edit"
      render :partial => "backup"
    else
      # 知見文書の save
#      begin
      unless @knowledge.save(@type)
        raise "failed to save knowledge: #{@knowledge.errors.inspect}"
      end
#      rescue
#        flash[:notice] = "Failed to save the document. Probably some data is not inputed.\n"
#      end

      # === 成功の場合。
      # Create, Save, Save As, new_from_analysis, comment

      # temporary backupを消す
      temporary_backups = KnowledgeBackup.find(:all, :conditions => ["backup_on=? AND temporary", @knowledge.path])
      temporary_backups.each do |tb|
        tb.destroy
      end

      # メッセージを出す
      if @type == "comment"
        flash[:notice] = "A comment is successfully saved."
      else
        flash[:notice] = "A knowledge document is successfully saved."
      end

      # 上書き保存のとき、バックアップを別テーブルに保存しておく
      if @type == "edit_save"
        create_backup
      end

      #redirect_to :action => 'show', :path => @knowledge.path, :from => 'create'
      redirect_to :action => 'show', :path => @knowledge.path, :from => 'update'
    end
=end

=begin
  # == 知見文書のバックアップを作成する。
  # * knowledges テーブルのバックアップを knowledge_backups テーブルへ
  #   knowledge_figures テーブルのバックアップを knowledge_figure_backups テーブルへ。
  # * バックアップは2種類ある。
  #   * 一時保存 : フォームの "Temporary Save" ボタンを押したときに作られる。
  #              フォームに入力された値のバックアップを保存する。.knlgeファイルは無し。
  #   * 変更履歴 : 編集機能で、上書き保存 ("Save") されたときに作られる。
  #              .knlgeファイルも作成され、中身がそこに書かれる。
  # * params[commit]が "Save" か "Temporary Save" のときに呼ばれる。
  def create_backup
    # === determine version_number of backup.
    if @type == "temporary_save"
      saved_backup = @knowledge
      new_version_number = 0
    else
      saved_backup = @old_knowledge
      backups = KnowledgeBackup.find(:all, :conditions => ["backup_on=?", @knowledge.path])
      if backups.length > 0
        backups.collect! {|backup|
          backup = backup.version_number
        }
        new_version_number = backups.max + 1
      else
        new_version_number = 1
      end
    end
    
    # === knowledge_backups Table.
    knowledge_backup = KnowledgeBackup.copy_from_knowledge(@knowledge, @type, @user, @groups, new_version_number)
    @knowledge.id = params[:original_id] if @type == "temporary_save"
    
    # === knowledge_figure_backups Table.
    knowledge_figure_backups = Array.new
    if params[:knowledge_figure]
      params[:knowledge_figure].sort {|a, b| a[0].to_i <=> b[0].to_i}.each do |key, value| # 絵の順番を正しくソート
        unless (value["figure_path"] == "" && value["figure_caption"] == "")
          # * バックアップの場合は、必ず既存の画像なので find で見つかるはず
          unless img = Image.find(:first, :conditions => ["path=?", value["figure_path"] + ".png"], :user => @user)
            raise "Image file #{figure_path} is not found."
          end
          knowledge_figure_backup = KnowledgeFigureBackup.new(:caption => value["figure_caption"], :image_path => img.path)
          knowledge_figure_backup.knowledge_backup = knowledge_backup
          knowledge_figure_backups.push(knowledge_figure_backup)
        end
      end
    end
    knowledge_backup.knowledge_figure_backups = knowledge_figure_backups
    unless knowledge_backup.save
      flash[:notice] = "The backup failed to save."
    end
    
    # === create ".knlge" file for backup.
    # バージョンつきのバックアップは、.knlgeファイルとして保存する
    unless @type == "temporary_save"
      # * knowledge_backups Table.
      knowledge_backup_hash = {"gfdnavi_knowledge" => saved_backup.get_contents}

      # * knowledge_figure_backups Table
      saved_backup.knowledge_figures.length.times do |i|
        image_path = saved_backup.knowledge_figures[i].image_path
        knowledge_backup_hash["gfdnavi_knowledge"]["knowledge_figures"].push({"image_path" => image_path, "caption"  => @old_knowledge.knowledge_figures[i].caption})
      end

      File.open(saved_backup.fname[0..-7] + "." + new_version_number.to_s + ".knlge","w"){|file| file.print knowledge_backup_hash.to_yaml}
    end
  end
=end
  
  # == 知見文書をデータベースとディスクから消去する
  def destroy_document
    user = (login=session[:user]) && User.find_by_login(login)
    knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:path]], :user => user)

    if params[:parent_path] # コメントを削除する場合      
#      knowledge.delete(user.login, params[:parent_path])
      knowledge.delete(user.login)
      @knowledge, @knowledge_figures, @comments, @image_paths, @image_widths, @image_heights, @caption_widths, @caption_heights = Knowledge.read_parameters_of_knowledge_document(params[:parent_path], @user)
      @displayed_knowledge = @knowledge
      flash[:notice] = "A comment is successfully deleted."
      render :partial => 'comments'
    else                    # コメントでない、普通の知見文書を削除する場合
      knowledge.delete(user.login)
      flash[:notice] = "A knowledge document is successfully deleted."
      redirect_to :action => 'list'
    end
  end

  # == 知見文書のバックアップを削除する。
  # * 削除すべきものは、DBのknowledge_backupsテーブル、knowledge_figure_backupsテーブルの中身。
  #   それから .knlge ファイル。(temporaryじゃ無いときだけ)
  # * create_backup は model に記述。controller の updateメソッドから呼ぶ。
  def destroy_backup
    # @user, @type は render 後に必要
    @user = (login=session[:user]) && User.find_by_login(login)
    @type = params[:type]
    knowledge_backup = KnowledgeBackup.find(params[:backup_id])
    knowledge_figure_backup = KnowledgeFigureBackup.new

    # edit の場合、@knowledge は render 後に必要
    if @type == "edit"
      n = Node.find(:first, :conditions => ["id=?", params[:document_id]], :user => @user)
      @knowledge = Knowledge.find(:first, :conditions => ["path=?", n.path], :user => @user)
    end

    # 一時保存でなく、編集のときは、 .knlgeファイルを消去する。
    unless knowledge_backup.temporary 
      knlge_file = Knowledge.find(params[:document_id], :user => @user)
      File.delete(knlge_file.fname[0..-7] + "." + knowledge_backup.version_number.to_s + ".knlge")
    end

    # データベースからの消去
    flash[:notice] = "A backup file is failed to delete." unless knowledge_backup.destroy

    render :partial => "backup"
  end

  
  # == show.rhtmlの下部にコメント作成用のフォームを出現させる
  def appear_comment_input_form
    # 出現するフォームは partial で埋め込まれているので、
    # 非ログイン時、 before_filter :login_required を使わずに
    # ログイン後は一旦 index (list.rhtml) まで戻ってもらう。
    unless @user = (login=session[:user]) && User.find_by_login(login)
      redirect_to :controller => "user", :action => "login"
    else
      # これらの変数は各コメントを表示する際に上書きされてしまっているので
      # ここで、コメントのつけられた、元のドキュメントの値を再び代入しておく。
      @knowledge, @knowledge_figures, @comments, @image_paths, @image_widths, @image_heights, @caption_widths, @caption_heights = Knowledge.read_parameters_of_knowledge_document(params[:path], @user)
      @num_of_figure = 1
      @type = "comment"

      # editと同様、@knowledge の中身がそのままフォームに入る。
      # しかし、コメントの場合はそのまま入っては困るので、ここで加工しておく。
      @new_comment_number = (@comments.collect{|comment| comment.comment_number}.max || 0) + 1
      @knowledge.title = "Re[#{@new_comment_number}]:"+ @knowledge.title
      @knowledge.creator = @user.full_name
      @knowledge.textbody = ""
      @knowledge.description = ""
      if @user.super_user
        @comment_path = @knowledge.path[1..-7] + "_comment_" + @new_comment_number.to_s
      else
        @comment_path = @knowledge.path.split(File::Separator)[-1][0..-7] + "_comment_" + @new_comment_number.to_s
      end
      render :partial => "comment_input_form"
    end
  end

  # == 知見ドキュメント作成フォーム内の、グループの可視性の入力部分を knowledge テーブルから書き戻す。
  def restore_group_form
    @user = (login=session[:user]) && User.find_by_login(login)
    @groups = @user.belonging_groups
    render :partial => "group_form"
  end

  # == 知見ドキュメント作成フォーム内の、図の入力部分を knowledge_figure_backups テーブルから書き戻す。
  #    ---- 未完成 ----
  def restore_knowledge_figure_form
    @type = params[:type]
    @user = (login=session[:user]) && User.find_by_login(login)
    @groups = @user.belonging_groups
    
    # list.rhtml -> edit.rhtml (Click "Edit".)
    @knowledge = KnowledgeBackup.find(params[:id])
=begin
    @figure_captions = Array.new
    @figure_paths = Array.new

    knowledge_figures = @knowledge.knowledge_figure_backups
    knowledge_figures.each do |kf|
      @figure_captions << kf.caption
      if (img = Image.find(:first, :conditions => ["path=?", kf.image_path], :user => @user))
        # 一時保存のときは、このimage_idがnullになる。
        @figure_paths << img.node.path
      else
        @figure_paths << kf.temporarily_image_path
      end
    end
=end
    @num_of_figure = @knowledge.knowledge_figure_backups.length

    render :partial => "knowledge_figure_form"
  end

  
  # == 存在するカテゴリーのリストを取り出す
  def category_search
    @categories = Array.new
    Knowledge.find(:all, :group => "category", :conditions => ['category LIKE ?', params[:keyword] + '%'], :order => "category").each do |knowledge|
      @categories << knowledge.category
    end
    
    render :partial => 'category_table'
  end

  # /view/knowledge/_layout_figure.rhtml で使用
  def fig2analysis
    user = (login=session[:user]) && User.find_by_login(login)
    analysis = Analysis.create_from_path(params[:org_path],user)
    session[:analysis] = analysis

    # 変数一覧に変数名を入れるために必要
    a_v = session[:variables_list] || Array.new
    a_v += analysis.variables.map{|v| v.path}
    a_v.uniq!
    session[:variables_list] = a_v
    session[:diagrams] = [params[:org_path]]
    redirect_to :controller => "analysis", :action => "index"
  end

  def fig2analysis2
    session[:diagrams] ||= []
    session[:diagrams].unshift(params[:org_path]) # add at the top
    redirect_to :controller => "analysis2", :action => "index"
  end

=begin
  private
  def make_directories(obj, user, groups, other_mode)
    full_path = ""
    parent = nil

    obj.path.split(File::Separator)[0..-2].each{|dname|
      full_path = File.join(full_path, dname)
      dir = Directory.find(:first, :conditions=>["path=?",full_path], :user=>user)
      unless dir
        dir = Directory.new
        dir.name = dname
        dir.path = full_path
        dir.parent = parent.node
        dir.owner = user
        dir.other_mode = other_mode
        dir.node.set_rgroups(groups) if groups.length > 0
        if full_path == obj.file
          dir.downloadable = obj.downloadable?
          dir.plain_file = true
        end
        dir.save!
      end
      parent = dir
    }
  end
=end
  
end


