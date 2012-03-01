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

  # == paginate�𗘗p���āA�m���h�L�������g�̈ꗗ��\������
  def list
    @user = (login=session[:user]) && User.find_by_login(login)
    
    # ���̕����̃R�[�h�͎d���Ȃ����G�ȍ��ɂȂ��Ă���B
    # paginate ���\�b�h�͓����I�� find ���\�b�h���g���Ă��邪
    # ���� find �� Rails �Œ�`���ꂽ�I���W�i���̂��̂ł���B
    # ����A�g�����������̂� node.rb �ōĒ�`���� find�B
    # �Ƃ������ƂŁA�d�����邪 node.rb �� find �Ɠ����悤�ȋL�q�������B
    # list_without_layout �ɂ��Ă����l�B
    other_readable_conditions = Node.conditions_to_read(@user)
    unless @user # �񃍃O�C�����
      conditions = " AND (#{other_readable_conditions})" # sqlite3, MySQL�̗����ɑΉ�
    else
      if @user.super_user
        conditions = ""
      elsif @user.groups == 0 # ���[�U���O���[�v�ɏ������Ȃ��ꍇ
        conditions = " AND (( (owner_id = #{@user.id}) OR (#{other_readable_conditions})))"
      else                    # ���[�U�����炩�̃O���[�v�ɏ�������ꍇ
        conditions = " AND (( (owner_id = #{@user.id}) OR NOT ((groups_readable & 1) = 0) OR (#{other_readable_conditions})))"
      end
    end
    
    # �\�[�g
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
    
    # �i�荞��
    if params["commit"] == "Search"
      conditions += " AND (knowledges.category = \"#{params['category']}\")" unless params["category"] == ""
      conditions += " AND (knowledges.creator = \"#{params['creator']}\")" unless params["creator"] == ""
    end
    
    # �y�[�W���O���ĕ\������
    @knowledges = Knowledge.paginate_by_sql("SELECT knowledges.* FROM nodes, knowledges WHERE ((knowledges.node_id=nodes.id) AND (node_type=3) #{conditions} AND (knowledges.comment_on IS NULL)) ORDER BY #{order}", :page => params[:page], :per_page => GFDNAVI_PAGINATE_PER)
  end
  
  # == ���̉�ʂɖ��ߍ��ނ��߂ɁA
  # ��ʏ㕔��Gfdnavi�̃��j���[���o������list��\������
  # �R�����g�̈ꗗ�\���ȂǂɎg����B
  def list_without_layout
    other_readable_conditions = Node.conditions_to_read(@user)
    @list_without_layout = true
    @user = (login=session[:user]) && User.find_by_login(login)
    if params[:node_ids]
      node_ids = params[:node_ids]
    end
    
    unless @user # �񃍃O�C�����
      conditions = " AND (#{other_readable_conditions})"
    else
      if @user.super_user
        conditions = ""
      elsif @user.groups == 0 # ���[�U���O���[�v�ɏ������Ȃ��ꍇ
        conditions = " AND (( (owner_id = #{@user.id}) OR (#{other_readable_conditions})))"
      else                    # ���[�U�����炩�̃O���[�v�ɏ�������ꍇ
        conditions = " AND (( (owner_id = #{@user.id}) OR NOT ((groups_readable & 1) = 0) OR (#{other_readable_conditions})))"
      end
    end

    # �R�����g�̏ꍇ�� comment_number �̏��A�����łȂ���ΐV�������ɕ\��
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
    # �Ƃ肠���� paginate �͂��Ȃ��B�G���[�ɂȂ�̂ŁB
#    @knowledges = Knowledge.find_by_sql("SELECT knowledges.* FROM nodes, knowledges WHERE ((node_id IN (#{node_ids_sql})) AND (knowledges.node_id=nodes.id) AND (node_type=3) AND ((other_readable))) ORDER BY #{order}")
#    @knowledges = Knowledge.paginate(:all, {:user => @user}, {:page => params[:page], :per_page => 3, :conditions => ["(node_id IN (?))", node_ids], :order => order })
    @knowledges = Knowledge.find(:all, :conditions => ["(node_id IN (?))", node_ids], :order => order, :user => @user)
    @not_paginate = true
    render :action => 'list'
  end

  # �m���h�L�������g���{������
  def show
    begin
      @user = (login=session[:user]) && User.find_by_login(login)
      @knowledge, @knowledge_figures, @comments, @image_paths, @image_widths, @image_heights, @caption_widths, @caption_heights = Knowledge.read_parameters_of_knowledge_document(params[:path], @user)
    rescue
      #unless params[:from] && params[:from] == "create"
      unless params[:from] && params[:from] == "update"
        # create������show�ɔ�΂��ꂽ�Ƃ��͈ȉ��̃��b�Z�[�W��\�������Ȃ�
        flash[:notice] = "The knowledge document doesn't exist."
      end
      redirect_to :action => 'list'
    else
      @num_of_figure = 1 # �R�����g���e�����o���������Ƃ��ɕK�v
    end
  end

  # ��ʏ㕔��Gfdnavi�̃��j���[���o������show�����s����
  def show_without_layout
    @show_without_layout = true
    show
    render :action => 'show'
  end

  # �m���h�L�������g���쐬���邽�߂̃X�N���v�g��\��������
  def script
    begin
      user = (login=session[:user]) && User.find_by_login(login)
      knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:path]], :user => user)
      @knowledge_script = knowledge.get_script(user)
    rescue => exception
      # ��̕����́Aknowledge.save �̓����ł�
      # �ۑ��O�� before_save ���\�b�h�����s����Ă���B
      # ��������オ���Ă�����O��ߑ�����B
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

    # ��͉�ʂŕ`���ꂽ�G�̃p�X���擾����
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
    
    # �ҏW�Ώۂ̕����̓��e�����炩���߃t�H�[���ɏ�������ł���
    @knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:path]], :user => @user)
    unless @user == @knowledge.owner || @user.super_user
      flash[:notice] = "Sorry, you don't have the right to edit this document."
      redirect_to :action => "list"
    end
    session[:knowledge] = Hash.new
    session[:knowledge_figure] = Hash.new
    @figure_captions = Array.new
    @figure_paths = Array.new
    
    # �������̐}�ɂ��Ă����l
    knowledge_figures = @knowledge.knowledge_figures
    knowledge_figures.each do |kf|
      img = Image.find(:first, :conditions => ["path = ?", kf.image_path], :user => @user)
      @figure_captions << kf.caption
      @figure_paths << img.path
    end

    @num_of_figure = @knowledge.knowledge_figures.length
  end

  # == create and save Knowledge Document
  # �ϐ� @knowledge �� knowledge_figures �ɓ��̓t�H�[������擾�����l��������
  # �Ō�� save (temporary save ���s��).
  # 
  # save, temporary save �̏ꍇ������
  # * �ꎞ�ۑ��Ȃ�o�b�N�A�b�v�̂ݕۑ�
  # * �㏑���ۑ��Ȃ�{�̂ƃo�b�N�A�b�v�̗�����ۑ�
  # * ���̂ق��̏ꍇ�͖{�̂̂ݕۑ�
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

    # ���͓��e�`�F�b�N
    unless @type == "temporary_save"
      if params["knowledge"]["category"] == ""
        category_error = "Category Error! Please input \"category\".\n"
      end
    end

    # === knowledges �e�[�u��
    # ==== Knowledge �I�u�W�F�N�g��p�ӁB�㏑���̂Ƃ������� find ���Ă��āA���� id ���g���B
    if @type=="edit" && @commit=="Save" # @type == "edit_save"
      knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:original_path]], :user => user)
      knowledge.presaved_node_relations = Array.new
      knowledge_copy = knowledge.dup
    else
      knowledge = Knowledge.new
    end

    # ==== �n�b�V�����璆�g�����Ă���
    k_hash = params["knowledge"]
    # owner �͏㏑���ۑ����͌��̂��̂����̂܂܎g�p����(�ʂ̐l�ɂȂ��Ă��܂�Ȃ��悤��)
    if @type=="edit" && @commit=="Save" # @type == "edit_save"
      k_hash["owner"] = knowledge_copy.owner
    else
      k_hash["owner"] = user.login
    end

    # �R�����g�̏ꍇ�́A�e�ƂȂ镶����id ���擾����
    if @type=="comment"
      unless parent_knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:path]], :user => user)
        raise "error. parent knowledge document not found.\n"
      end
#      print "parent_knowledge = "
#      pp parent_knowledge
    end

    # �R�����g�֘A�̃p�����[�^������(comment_on, comment_number)
    if @type == "comment"
      k_hash["comment_on"]     = parent_knowledge.path
      k_hash["comment_number"] = params["knowledge"]["comment_number"]

#      print "k_hash = "
#      pp k_hash
    end

    # �ǂ��ǂ������O���[�v�ʂɐݒ�
    # (��{�I��DB�ۑ��� models �ōs�����Argroups �� other_mode �����͂����ő�����Ă���)
    # * �R�����g�̏ꍇ�A�e�ƂȂ镶���̐ݒ�������p���B
    if @type == "comment"
      knowledge.rgroups = parent_knowledge.rgroups
      knowledge.other_mode = parent_knowledge.other_mode
    else
      if @type == "temporary_save" # temporary �Ȃ���e�ɂ��Ă̂�����͎󂯂Ȃ� 
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

    # �摜�֘A�̃p�����[�^������
    ### �{���� view �̕������O�����킹�Ă����āAparams���炻�̂܂܃R�s�[���邾���ōςނ悤�ɂ��ׂ�
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

    # layout��"figures in a row above text."�ɂ����ꍇ�͂��ꂪnull�ɂȂ��Ă���̂�
    unless k_hash["horizontal_figures"]
      k_hash["horizontal_figures"] = 1
    end
    k_hash.delete("percent_or_pixel")

    # === nodes �e�[�u��
    # path �����߂�B�㏑���̎������͊����̂��̂��g�p�B
    if @type=="edit" && @commit=="Save" # @type == "edit_save"
      path = params[:original_path]
    else
      # path �� dirname �����߂�B��ʃ��[�U�� /usr/ �ȉ��ɂ����t�@�C����u���Ȃ��B
      path = params["node"]["path"] + ".knlge"
      if user.super_user
        path = "/" + path
      else
        path = "/usr/"+ Knowledge.remove_scheme(user) + "/knowledge/" + path
      end
    end

    # �ҏW���̏㏑���ۑ���ꎞ�I�ȕۑ��̏ꍇ�A�p�X�̏d���Ȃǂ̓`�F�b�N���Ȃ��悤�ɂ���B
    # (model��before_save�Ő���)
    if (@type=="edit" && @commit=="Save") || (@type == "temporary_save") || (@type=="comment" && @commit=="Add a Comment on this document.")
      knowledge.ignore_check_path = true
    end

    # === knowledge_figures �e�[�u��
    #     �܂��}�𐳂������ԂɃ\�[�g����Bsort��Array��Ԃ��̂ŁA���Hash�ɖ߂��Ă��B
    kf_hash_ary = []
    if params["knowledge_figure"]
      sorted_kf = params["knowledge_figure"].sort {|a, b| a[0].to_i <=> b[0].to_i}
      sorted_kf.each {|key, value| kf_hash_ary.push value } # Hash �� Array �ɂ���
      kf_hash_ary.delete_if {|kf_hash| kf_hash["figure_path"] == ""} # path ���������͍̂폜
    end

    notfound_error = ""
    # kf_hash �Ɋ��҂���Ă���̂́Aimage_path �� caption �̂�
    kf_hash_ary.each_with_index do |kf_hash, index|
      # �摜��path��Hash�Ɋi�[����B
      # path������ = �܂��ۑ�����Ă��Ȃ��@�ꍇ�͂܂��摜��ۑ�����
      #
      # new_from_analysis �ȊO�� ���� find �� nil �ɂȂ�Ƃ�����
      # ���݂��Ȃ��摜�̃p�X��\�����Ƃ��B

      if image = Image.find(:first, :conditions => ["path=?", kf_hash["figure_path"] + ".png"], :user => user)
        # �����̉摜�̏ꍇ�͂���path�𗘗p
        image_path = image.path
      else
        # �V�K�̉摜�Ƃ݂Ȃ�(kf_hash["figure_path"]�ɂ� org_path �������Ă���)
        if @type=="new_from_analysis"
          gd = NumRu::GfdnaviData.open(kf_hash["figure_path"], user)
          image_path = knowledge.image_save(path[0..-7], index, kf_hash["name"], gd, user)
        else
          # new_from_analysis�ȊO�̏ꍇ�͐V�K�̉摜�͓���Ȃ��B�p�X���Ԉ���Ă���B
          notfound_error += "Image not found."
        end
      end

      # image_path �� nil �Ȃ�p�X(kf_hash["figure_path"])���Ԉ���Ă���Ƃ��ăG���[
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

    # �摜��������Ȃ������ꍇ�̓G���[�ɂ���
    unless notfound_error == ""
      flash[:notice] = notfound_error
      redirect_to :action => 'list'
    else
      comment_hash = nil
      refer_hash_ary = Array.new
      # === �ۑ�
      begin
        Knowledge.transaction do
          knowledge.delete_node_relations if @type=="edit" && @commit=="Save" # @type == "edit_save"
          knowledge.read_knowledge_hash(path, k_hash, size=nil)
          knowledge.read_knowledge_figure_hash(kf_hash_ary)

          # @knowledge, @user �� render��ɕK�v
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
        # ��̕����́Aknowledge.save �̓����ł�
        # �ۑ��O�� before_save ���\�b�h�����s����Ă���B
        # ��������オ���Ă�����O��ߑ�����B
        flash[:notice] = exception
        redirect_to :action => 'list'       
      end
    end
  end

  # �R�����g�������ꂽ���render���邽�߁B
  def comment_written
    @user = (login=session[:user]) && User.find_by_login(login)
    @knowledge, @knowledge_figures, @comments, @image_paths, @image_widths, @image_heights, @caption_widths, @caption_heights = Knowledge.read_parameters_of_knowledge_document(params[:path], @user)
    @num_of_figure = 1 # �R�����g���e�����o���������Ƃ��ɕK�v
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
    #   * �㏑�����ɂ́A���̃p�X��p����
    #   * ����ȊO�̎��́A���͂��ꂽ�p�X��p����
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
    
    # �㏑���ۑ����A���Ƃ����̃��[�U�ƕʂ̃��[�U(�X�[�p�[���[�U�Ȃ�)�������������Ƃ��Ă��Aowner�͕ύX���Ȃ�
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
      params[:knowledge_figure].sort {|a, b| a[0].to_i <=> b[0].to_i}.each do |key, value| # �G�̏��Ԃ𐳂����\�[�g
        unless (value["figure_path"] == "" && value["figure_caption"] == "")
          if image = Image.find(:first, :conditions => ["path=?", value["figure_path"] + ".png"], :user => @user)
            # * �����̉摜�̏ꍇ (find ���āA������Ί����̉摜�Ƃ݂Ȃ�)
            fig_args.push({"image" => image.path, "caption" => value["figure_caption"]})
            figure_paths.push(image.path) # ���node_relations�e�[�u����ۑ�����Ƃ��Ɏg��
          else
            # * Gfdnavi���ō쐬�����摜�Ƌ��ɕ�����ۑ�����ꍇ
            gfdnavi_data = NumRu::GfdnaviData.open(value["figure_path"])
            fig_args.push({"image" => gfdnavi_data, "caption" => value["figure_caption"]})
          end
        end # end of unless
      end # end of each
    end # end of (if params[:knowledge_figure])
    fig_args.push(@user.login)

    @knowledge.insert_figures = fig_args

    # �����p�ɁAtitle �� textbody �� author(creator) �� category �� keyword_attributes �ɓ����B
    @knowledge.keyword_attributes.build(:name => "title", :value => params[:knowledge]["title"])
    @knowledge.keyword_attributes.build(:name => "textbody", :value => params[:knowledge]["textbody"])
    @knowledge.keyword_attributes.build(:name => "description", :value => params[:knowledge]["description"])
    @knowledge.keyword_attributes.build(:name => "creator", :value => params[:knowledge]["creator"])
    @knowledge.keyword_attributes.build(:name => "category", :value => params[:knowledge]["category"])

    # �X�V���Amtime���㏑��
    @knowledge.mtime = Time.new unless params[:commit] == "Create"
    
    # * �ꎞ�ۑ��Ȃ�o�b�N�A�b�v�̂ݕۑ�
    # * �㏑���ۑ��Ȃ�{�̂ƃo�b�N�A�b�v�̗�����ۑ�
    # * ���̂ق��̏ꍇ�͖{�̂̂ݕۑ�
    if @type == "temporary_save"
      create_backup
      @type = params[:type]
      # �㏑���ۑ����̃o�b�N�A�b�v�����̂Ƃ��́A���f�[�^��ID���e�[�u���ɓ����
      @knowledge = Knowledge.find(params[:document_id], :user => @user) if @type == "edit"
      render :partial => "backup"
    else
      # �m�������� save
#      begin
      unless @knowledge.save(@type)
        raise "failed to save knowledge: #{@knowledge.errors.inspect}"
      end
#      rescue
#        flash[:notice] = "Failed to save the document. Probably some data is not inputed.\n"
#      end

      # === �����̏ꍇ�B
      # Create, Save, Save As, new_from_analysis, comment

      # temporary backup������
      temporary_backups = KnowledgeBackup.find(:all, :conditions => ["backup_on=? AND temporary", @knowledge.path])
      temporary_backups.each do |tb|
        tb.destroy
      end

      # ���b�Z�[�W���o��
      if @type == "comment"
        flash[:notice] = "A comment is successfully saved."
      else
        flash[:notice] = "A knowledge document is successfully saved."
      end

      # �㏑���ۑ��̂Ƃ��A�o�b�N�A�b�v��ʃe�[�u���ɕۑ����Ă���
      if @type == "edit_save"
        create_backup
      end

      #redirect_to :action => 'show', :path => @knowledge.path, :from => 'create'
      redirect_to :action => 'show', :path => @knowledge.path, :from => 'update'
    end
=end

=begin
  # == �m�������̃o�b�N�A�b�v���쐬����B
  # * knowledges �e�[�u���̃o�b�N�A�b�v�� knowledge_backups �e�[�u����
  #   knowledge_figures �e�[�u���̃o�b�N�A�b�v�� knowledge_figure_backups �e�[�u���ցB
  # * �o�b�N�A�b�v��2��ނ���B
  #   * �ꎞ�ۑ� : �t�H�[���� "Temporary Save" �{�^�����������Ƃ��ɍ����B
  #              �t�H�[���ɓ��͂��ꂽ�l�̃o�b�N�A�b�v��ۑ�����B.knlge�t�@�C���͖����B
  #   * �ύX���� : �ҏW�@�\�ŁA�㏑���ۑ� ("Save") ���ꂽ�Ƃ��ɍ����B
  #              .knlge�t�@�C�����쐬����A���g�������ɏ������B
  # * params[commit]�� "Save" �� "Temporary Save" �̂Ƃ��ɌĂ΂��B
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
      params[:knowledge_figure].sort {|a, b| a[0].to_i <=> b[0].to_i}.each do |key, value| # �G�̏��Ԃ𐳂����\�[�g
        unless (value["figure_path"] == "" && value["figure_caption"] == "")
          # * �o�b�N�A�b�v�̏ꍇ�́A�K�������̉摜�Ȃ̂� find �Ō�����͂�
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
    # �o�[�W�������̃o�b�N�A�b�v�́A.knlge�t�@�C���Ƃ��ĕۑ�����
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
  
  # == �m���������f�[�^�x�[�X�ƃf�B�X�N�����������
  def destroy_document
    user = (login=session[:user]) && User.find_by_login(login)
    knowledge = Knowledge.find(:first, :conditions => ["path=?", params[:path]], :user => user)

    if params[:parent_path] # �R�����g���폜����ꍇ      
#      knowledge.delete(user.login, params[:parent_path])
      knowledge.delete(user.login)
      @knowledge, @knowledge_figures, @comments, @image_paths, @image_widths, @image_heights, @caption_widths, @caption_heights = Knowledge.read_parameters_of_knowledge_document(params[:parent_path], @user)
      @displayed_knowledge = @knowledge
      flash[:notice] = "A comment is successfully deleted."
      render :partial => 'comments'
    else                    # �R�����g�łȂ��A���ʂ̒m���������폜����ꍇ
      knowledge.delete(user.login)
      flash[:notice] = "A knowledge document is successfully deleted."
      redirect_to :action => 'list'
    end
  end

  # == �m�������̃o�b�N�A�b�v���폜����B
  # * �폜���ׂ����̂́ADB��knowledge_backups�e�[�u���Aknowledge_figure_backups�e�[�u���̒��g�B
  #   ���ꂩ�� .knlge �t�@�C���B(temporary���ᖳ���Ƃ�����)
  # * create_backup �� model �ɋL�q�Bcontroller �� update���\�b�h����ĂԁB
  def destroy_backup
    # @user, @type �� render ��ɕK�v
    @user = (login=session[:user]) && User.find_by_login(login)
    @type = params[:type]
    knowledge_backup = KnowledgeBackup.find(params[:backup_id])
    knowledge_figure_backup = KnowledgeFigureBackup.new

    # edit �̏ꍇ�A@knowledge �� render ��ɕK�v
    if @type == "edit"
      n = Node.find(:first, :conditions => ["id=?", params[:document_id]], :user => @user)
      @knowledge = Knowledge.find(:first, :conditions => ["path=?", n.path], :user => @user)
    end

    # �ꎞ�ۑ��łȂ��A�ҏW�̂Ƃ��́A .knlge�t�@�C������������B
    unless knowledge_backup.temporary 
      knlge_file = Knowledge.find(params[:document_id], :user => @user)
      File.delete(knlge_file.fname[0..-7] + "." + knowledge_backup.version_number.to_s + ".knlge")
    end

    # �f�[�^�x�[�X����̏���
    flash[:notice] = "A backup file is failed to delete." unless knowledge_backup.destroy

    render :partial => "backup"
  end

  
  # == show.rhtml�̉����ɃR�����g�쐬�p�̃t�H�[�����o��������
  def appear_comment_input_form
    # �o������t�H�[���� partial �Ŗ��ߍ��܂�Ă���̂ŁA
    # �񃍃O�C�����A before_filter :login_required ���g�킸��
    # ���O�C����͈�U index (list.rhtml) �܂Ŗ߂��Ă��炤�B
    unless @user = (login=session[:user]) && User.find_by_login(login)
      redirect_to :controller => "user", :action => "login"
    else
      # �����̕ϐ��͊e�R�����g��\������ۂɏ㏑������Ă��܂��Ă���̂�
      # �����ŁA�R�����g�̂���ꂽ�A���̃h�L�������g�̒l���Ăё�����Ă����B
      @knowledge, @knowledge_figures, @comments, @image_paths, @image_widths, @image_heights, @caption_widths, @caption_heights = Knowledge.read_parameters_of_knowledge_document(params[:path], @user)
      @num_of_figure = 1
      @type = "comment"

      # edit�Ɠ��l�A@knowledge �̒��g�����̂܂܃t�H�[���ɓ���B
      # �������A�R�����g�̏ꍇ�͂��̂܂ܓ����Ă͍���̂ŁA�����ŉ��H���Ă����B
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

  # == �m���h�L�������g�쐬�t�H�[�����́A�O���[�v�̉����̓��͕����� knowledge �e�[�u�����珑���߂��B
  def restore_group_form
    @user = (login=session[:user]) && User.find_by_login(login)
    @groups = @user.belonging_groups
    render :partial => "group_form"
  end

  # == �m���h�L�������g�쐬�t�H�[�����́A�}�̓��͕����� knowledge_figure_backups �e�[�u�����珑���߂��B
  #    ---- ������ ----
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
        # �ꎞ�ۑ��̂Ƃ��́A����image_id��null�ɂȂ�B
        @figure_paths << img.node.path
      else
        @figure_paths << kf.temporarily_image_path
      end
    end
=end
    @num_of_figure = @knowledge.knowledge_figure_backups.length

    render :partial => "knowledge_figure_form"
  end

  
  # == ���݂���J�e�S���[�̃��X�g�����o��
  def category_search
    @categories = Array.new
    Knowledge.find(:all, :group => "category", :conditions => ['category LIKE ?', params[:keyword] + '%'], :order => "category").each do |knowledge|
      @categories << knowledge.category
    end
    
    render :partial => 'category_table'
  end

  # /view/knowledge/_layout_figure.rhtml �Ŏg�p
  def fig2analysis
    user = (login=session[:user]) && User.find_by_login(login)
    analysis = Analysis.create_from_path(params[:org_path],user)
    session[:analysis] = analysis

    # �ϐ��ꗗ�ɕϐ��������邽�߂ɕK�v
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


