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

  # == �m�������̕ۑ����A���̕ϐ������Đ}��}������
  @@to_saved_figures = Array.new # GfdnaviData�I�u�W�F�N�g�̔z��
  @@to_saved_figures_caption = Array.new

  # == �����̏����擾���AHash�ɂ��ĕԂ�
  #    (.knlge�t�@�C���ɒ��g������Ƃ��ƁA
  #    save_as��GfdnaviData.new����Ƃ��Ɏg���Ă���)
  #    ...������GfdnaviData.new���Ă������́H�悭�Ȃ��C���B
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

  # == �������쐬���邽�߂̃X�N���v�g��Ԃ�
  #    ��{�I�ɂ�get_contents�𗘗p���Đ��`�B
  #    owner�̂Ƃ��낾���̓X�N���v�g���p�҂̂��Ƃ��l���āA
  #    ��������s�������[�U�̖��O�ɂ���B
  def get_script(user)
    # knowledge �e�[�u��
    k_hash = self.get_contents
    pp k_hash
    k_str = ""
    k_str += "knowledge.url=url_prefix+\"#{self.path}\"\n"
    k_hash.each do |key, value|
      case key
      when "groups_readable","other_readable","other_mode","default_layout","figures_size_height_or_width","horizontal_figures","figures_size_number","figures_size_units"
        k_str += "knowledge.#{key}=#{value}\n"
      when "owner" # ���[�U������ owner �ł͂Ȃ� user �Ƃ������O�ɂ��Ă���̂�
        k_str += "knowledge.user=\"#{user.login}\"\n"
      when "name","knowledge_figures","comment_number","comment_on"
        next
      else
        k_str += "knowledge.#{key}=\"#{value}\"\n"
      end
    end

    # knowledge_figures �e�[�u��
    self.knowledge_figures.each do |kf|
      if (image = Image.find(:first, :conditions => ["path=?", kf.image_path], :user => user)) && image.org_path
        k_str += "knowledge.add_figure(\"#{image.org_path}\",\"#{kf.caption}\")\n"
      else
        k_str += "knowledge.add_figure(\"#{kf.image_path}\",\"#{kf.caption}\")\n"
      end
    end

    return k_str
  end

  # == �����Ɋ܂܂��}�́A���ɂȂ����ϐ����擾����
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

  # == �����Ɋ܂܂��}�́A�摜���擾����
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

  # == self.knowledge_figures �̒��g��ݒ肷��(CUI)
  #    * �����͉摜�� path �� caption �� YAML���������̂�
  #      path1, path2, ... , pathN, caption1, caption2, ... , captionN
  #      �ƂȂ邱�Ƃ�z�肵�Ă���B
  #    * �Ȃ��Aknowledge_remote.rb �ł� add_figure ���\�b�h��p����1�����ǉ�����d�l�ɂȂ��Ă���B
  #      (�����Œǉ����ꂽ���̂��A�ŏI�I�ɂ܂Ƃ߂��Ă��� figures= ���\�b�h�̈����ɂȂ�)
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

      # * �����̉摜���g���ꍇ
      if image = Image.find(:first, :conditions => ["path=?", path], :user => self.owner)
        image_path = path
        kf = KnowledgeFigure.new
        kf.caption = NKF.nkf('-w', captions[index]) # UTF-8�֕����R�[�h�ϊ�
        kf.image_path = image_path
        kf.knowledge = self # knowledges�e�[�u���� id �Ŋ֘A�t����
        kfs.push(kf)
      else
        #path = "/" + $1  if (path =~ /\/data\/(.*)$/) # http://...../data �̕��������
        # * ���ۑ�����ꍇ
        ary_local = NumRu::GfdnaviData.open(path, self.owner) # GfdnaviData::ArrayLocal
        unless ary_local.is_a?(NumRu::GfdnaviData::Array)
          ary_local = NumRu::GfdnaviData::Array[ary_local]
        end
        ary_local.each do |img_local|
          # GfdnaviData::ImageLocal �� image_save �ɓn���A�摜�t�@�C����ۑ�(DB�ƃf�B�X�N�̗���)
          image_path = self.image_save(self.path, index, "", img_local, self.owner)
          kf = KnowledgeFigure.new
          kf.caption = NKF.nkf('-w', captions[index]) # UTF-8�֕����R�[�h�ϊ�
          kf.image_path = image_path
          kf.knowledge = self # knowledges�e�[�u���� id �Ŋ֘A�t����
          kfs.push(kf)
        end
      end
    end

    self.knowledge_figures = kfs
  end

=begin
    # URL����GfdnaviData�I�u�W�F�N�g�𐶐����A@@to_saved_figures �ɑ��
    paths.each do |path|
      #path = "/" + $1  if (path =~ /\/data\/(.*)$/) # http://...../data �̕��������
      image = NumRu::GfdnaviData.open(path) # GfdnaviData::ArrayLocal
      image.each do |img|
        @@to_saved_figures.push img     # GfdnaviData::ImageLocal
      end
    end
    @@to_saved_figures_caption += captions
  end
=end

  # == �������Ɋ܂܂��}�̏����擾���AHash��Array�ɂ��ĕԂ�
  def figures
    fig_infos = Array.new
    self.knowledge_figures.each do |kf|
      fig_infos.push({"image" => kf.image_path, "caption" => kf.caption})
    end
    return fig_infos # ����͂��̂܂� insert_figures= ���\�b�h�ɓn�����Ƃ��ł���
  end
  
  # == �����ɐ}��}������
  def insert_figures=(args)
    # args �� GfdnaviData, String (path), �������͂��̔z�� + ���[�U��
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

  # == (���ɕۑ����ꂽ�����ɑ΂���)��������}���폜���ĕۑ�����
  #    1�Ԗڂ̊G��1�Ŏw�肷��(0�ł͂Ȃ�)
  def delete_figure(arg, user_name)
    unless self.node_id
      raise "This method can be used for saved documents.\n"
    else
      figs = self.figures
      unless figs.delete_at(arg-1) # arg���͈͊O��������nil���Ԃ�
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

  # == (���ɕۑ����ꂽ�����ɑ΂���)�}�̏��Ԃ����ւ��ĕۑ�����
  #    1�Ԗڂ̊G��1�Ŏw�肷��(0�ł͂Ȃ�)
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

  # �������{�����錠���̂���O���[�v�� Array �Ŏ擾����
  # ����� node.rb �ɍ�����B(node_entity_abstract.rb����ڏ�)
  # def visibility
  #   Group.find_by_bit_flag(self.rgroups).collect{|g| g.name}
  # end
  
  # �R�����g���擾���AArray�ɂ��ĕԂ�
  def comments(user_name)
    user = User.find_by_login(user_name)
    comments = Knowledge.find(:all, :conditions=>["comment_on=?", self.node_id], :user => user)
  end

  ##### ���̃��X�g�����āA��̃R�����g��I��ł���ɂ���ɃR�����g�ł���Ƃ����B ####

  # self �ɑ΂��ẴR�����g���쐬����B
  def make_new_comment(user_name)
    user = (login=user_name) && User.find_by_login(login)
    
    # �����ŃR�����g�ԍ������߂�
    knowledge, knowledge_figures, comments, image_paths, image_widths, image_heights, caption_widths, caption_heights = Knowledge.read_parameters_of_knowledge_document(self.path, user)
    new_comment_number = (comments.collect{|comment| comment.comment_number}.max || 0) + 1

    # �����ŃR�����g�̃p�X�����߂�(��΃p�X�ŕ\������)
    comment_path = knowledge.path[0..-7] + "_comment_" + new_comment_number.to_s + ".knlge"

    # �R�����g�� new ���� return ����
    nc = NumRu::GfdnaviData.new(comment_path, user.login, "knlge")
    nc.comment_on = self.node_id # �R�����g���ɂȂ���Knowledge��node_id���i�[
    nc.comment_number = new_comment_number # ���Ԗڂ̃R�����g��
    nc.title = "Re[#{new_comment_number}]:"+ knowledge.title
    nc.category = "comment"
    return nc
  end

  # == �m�������̍폜
  # * �R�����g�������Ƃ��ɂ͌������̃p�X�������Ƃ��ēn��
  # * destroy����DB����������\�b�h�Ɩ��O�����̂ł���ɂ���
  def delete(user_name)
    kb_filenames = Array.new

    Knowledge.transaction do
      user = User.find_by_login(user_name)

      # === Knowledge(�R�����g)������
      comments = Knowledge.find(:all, :conditions => ["comment_on=?", self.node_id])
      comments.each do |comment|
        File.delete(comment.fname)
        comment.destroy
      end

      # === NodeRelation �̏���
      node_relations = NodeRelation.find(:all, :conditions => ["name=? AND referenced_by=?", "based_on", self.node_id])
      node_relations.each {|nr| nr.destroy }

      # === KnowledgeBackup, KnowledgeFigureBackup �̏���
      knowledge_backups = KnowledgeBackup.find(:all, :conditions => ["backup_on=?", self.path])
      knowledge_backups.each do |kb|
        kb_fname = File.dirname(self.fname)+"/"+File.basename(self.fname, ".knlge")+"."+kb.version_number.to_s+".knlge"
        kb_filenames.push kb_fname
        kb.destroy
      end

      # === Knowledge (�{��)���f�[�^�x�[�X�������
      self.destroy
    end

    # �f�B�X�N���� .knlge �t�@�C��������
    File.delete(self.fname) if File.exist?(self.fname)
    # �f�B�X�N���� .knlge �t�@�C��(�o�b�N�A�b�v)������
    kb_filenames.each do |fname|
      File.delete(fname) if File.exist?(fname)
    end

    # �f�B�X�N���� .png �t�@�C���Ƃ�����i�[���Ă���f�B���N�g��������(���݂���Ȃ��)
    sub_dirname = File.dirname(self.fname) + "/." + File.basename(self.fname, ".knlge")
    if File.exist?(sub_dirname)
      # �f�B���N�g�����̃t�@�C��������
      Dir::glob(sub_dirname + "/*.png*").each do |png|
        File.delete(png)
      end
      # �f�B���N�g�����̂�����
      Dir.rmdir(sub_dirname)
    end
  end

  # == .knlge�t�@�C������m�������𕜋�����(DB�ɏ�������)
  def restore(path, k_hash, size)
    # * Knowledge(DB�o�^)
    Knowledge.transaction do
      self.read_knowledge_hash(path, k_hash, size=nil)
      self.save! # Knowledge.save (write to DB)
    end
    # * KnowledgeFigure(DB�o�^), NodeRelations(DB�o�^) �͕ʓr db/register_directory_tree.rb �ŁB
    #   (Knowledge, Image, Node  etc... ���ۑ����ꂽ��ł܂Ƃ߂Ă���Ă���)
    # * .knlge�t�@�C���ւ̏������݂͍s��Ȃ�
  end

  # == �m��������DB�ɕۑ�����(�t�@�C��(.knlge)�ւ̏����o���͕ʃ��\�b�h)
  #    (GUI, �f�B���N�g���X�L����)
  def read_knowledge_hash(path, k_hash, size=nil)
    # === knowledge �e�[�u���̒��g��ݒ�
    k_hash.each do |key, value|
      # ���̑��Atextbody�Ƃ���KeywordAttribute�ɂ�����Ȃ��Ƃ����Ȃ������͂��B
      unless value
        # print "skip: #{key} because #{value} is nil.\n"
      else
        case key
        when "name", "path", "mtime", "size" # �������Ȃ�
        when 'owner','user'                  # 'user' is for backward compatibility
          if (user = User.find_by_login(value))
            self.owner = user
          end
        when 'comment_on'
          # �Ȃɂ����Ȃ�
        else
          eval("self.#{key} = value") # ���̂܂܂̌^�ŉ���
        end
      end
    end

    if comment_path = k_hash["comment_on"]
      parent_knowledge = Knowledge.find(:first, :conditions => ["path=?", comment_path], :user => self.owner)
      self.comment_on = parent_knowledge.node_id
    end

    # === k_hash �ɓ����Ă��Ȃ��v�f�ƁA
    #     node�e�[�u���̗v�f(�̈ꕔ)�͂����œ����
=begin
    unless self.ignore_check_path
      # GUI�̏ꍇ�͂����ɗ���܂łɃG���[�ɂȂ���redirect
      # CUI�̏ꍇ�͂�����raise����
      
      # path �̕�����`�F�b�N
      error = check_path(path)
      if error
        raise error
      end

      # ����path���������`�F�b�N
      if Knowledge.find(:first, :conditions => ["path=?", self.path], :user => :all)
        raise "Failure to save!!! Inputted path \"#{self.path}\" already exists.\n"
      end
    end
=end
    self.path = path
    self.size = size
    self.mtime = Time.new
    self.name = self.title
    # knowledge �� parent ��T���A������΍��
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

  # == self.knowledge_figures �̒��g��ݒ肷��(GUI)
  def read_knowledge_figure_hash(kf_hash_ary)
    knlge_figs = Array.new
    kf_hash_ary.each do |kf_hash|
      kf = KnowledgeFigure.new
      kf.caption = NKF.nkf('-w', kf_hash["caption"]) # UTF-8�֕����R�[�h�ϊ�
      kf.image_path = kf_hash["image_path"]
      kf.knowledge = self # knowledges�e�[�u���� id �Ŋ֘A�t����
      knlge_figs.push(kf)
    end
    self.knowledge_figures = knlge_figs
  end

  # == �ۑ��O�̃`�F�b�N
  #    * path
  #    * input_default_value
  #    * UTF-8�֕����R�[�h�ϊ�
  def before_save
    # path check(GUI����̕ۑ��̏ꍇ�A�㏑���ƈꎞ�ۑ��̂Ƃ��̓`�F�b�N���������)
    # �{���̓`�F�b�N������ׂ��ł͂Ȃ����낤 (����v�c�_)
    # �V�K�쐬�łȂ��A�X�V��R�����g�̂Ƃ��ɁA�������� path ���ύX����邱�Ƃ͋֎~���ׂ���������Ȃ�
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
      when /\.\d+\.knlge$/ # .����.knlge
        raise "Path Error! You input #{self.path}. Path like \"*****.number.knlge\" is forbidden."
      when /\/\s*.knlge$/ # /.knlge
        raise "Path Error! You input #{self.path}. Directory that doesn't have name is forbidden."
      when /\/\s*\// # // (ActiveRecord�������̔���Ɋ֌W�Ȃ��͂����Ă����݂���)
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
    
    # UTF-8�֕����R�[�h�ϊ�
    ["textbody", "title", "creator", "description"].each do |column|
      eval("self.#{column} = NKF.nkf('-w', self.#{column})")
    end
  end

  # === node_relations �e�[�u���̒��g������
  #     (Knowledge �� node �� id ���K�v�Ȃ̂ŁAKnowledge �� save ��Ɏg������)
  #     (1) ������ Hash �𗘗p���� Knowledge <-> Knowledge(�R�����g)�̊֌W��DB�o�^�B
  #     (2) self.knowledge_figures ���� Knowledge - Image, Knowledge - Variable �̊֌W��DB�o�^�B
  #     * (2)�̏���Hash�ɂ��A(1)��Hash�ɉ����� return (.knlge�t�@�C���������ݎ��ɗ��p)
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
      #   Image <-> Variable �̊֘A������ node_relations �e�[�u���ɂ���Ȃ�A�����������Ă���B
      var_nrs = NodeRelation.find(:all, :conditions => ["referenced_by=? AND name='draw'", image.node])

      if var_nrs.length > 0
        var_nrs.each do |nr|
          var_node_ids.push nr.reference
        end
      elsif (image.org_path && vd = NumRu::GfdnaviData.open(image.org_path, self.owner).get_object)
        # image.org_path ��������Ή摜��Variable�Ɩ��֌W�B
        # vd �� nil �Ȃ�摜��Variable�Ɩ��֌W�B
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

  # == �m���������t�@�C��(.knlge)�֏����o��(DB�ւ̕ۑ��͕ʃ��\�b�h)
  #    �^����ꂽ�����ɏ]���A�摜�������ɕۑ�����
  def save_knlge_file
#    print "[save_knlge_file]\n"

    unless self.ignore_save_knlge
      knowledge_hash = Hash.new
      knowledge_hash = {"gfdnavi_knowledge" => self.get_contents} # knowledges �e�[�u��
      # knowledge_figures �e�[�u��
      self.knowledge_figures.each do |kf|
        knowledge_hash["gfdnavi_knowledge"]["knowledge_figures"].push({"caption"=>kf.caption, "image_path"=>kf.image_path})
      end
      knowledge_hash["gfdnavi_knowledge"]["reference"] = self.presaved_node_relations # node_relations �e�[�u��
      File.open(self.fname,"w"){|file| file.print knowledge_hash.to_yaml}
    end

#    print "[end of save_knlge_file]\n"
  end


  # "self ����Q�Ƃ���Ă���" �Ƃ�����������
  def delete_node_relations
    nrs = NodeRelations.find(:first, :conditions => ["referenced_by=?", self.node], :user => self.owner)
    nrs.each {|nr| nr.delete}
  end


  # == �m�������̃o�b�N�A�b�v���쐬����B
  # * knowledges �e�[�u���̃o�b�N�A�b�v�� knowledge_backups �e�[�u����
  #   knowledge_figures �e�[�u���̃o�b�N�A�b�v�� knowledge_figure_backups �e�[�u���ցB
  # * .knlge�t�@�C�����o�[�W�����ԍ����Ŏc��
  # 
  # * params[commit]�� "Save" �� "Temporary Save" �̂Ƃ��ɌĂ΂��B
  # * �o�b�N�A�b�v��2��ނ���B
  #   * �ꎞ�ۑ� : �t�H�[���� "Temporary Save" �{�^�����������Ƃ��ɍ����B
  #              �t�H�[���ɓ��͂��ꂽ�l�̃o�b�N�A�b�v��ۑ�����B.knlge�t�@�C���͖����B
  #   * �ύX���� : �ҏW�@�\�ŁA�㏑���ۑ� ("Save") ���ꂽ�Ƃ��ɍ����B
  #              .knlge�t�@�C�����쐬����A���g�������ɏ������B
  # * destroy_backup �� controller �ɋL�q�BGUI���璼�ڌĂԁB
  def create_backup(type, kf_hash_ary, groups)
    # === �o�b�N�A�b�v�̃o�[�W�����ԍ������߂�
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
      kf.caption = NKF.nkf('-w', kf_hash["caption"]) # UTF-8�֕����R�[�h�ϊ�
      kf.image_path = kf_hash["image_path"]
      kf.knowledge_backup = knowledge_backup # knowledge_backups�e�[�u���� id �Ŋ֘A�t����
      knlge_figs.push(kf)
    end
    knowledge_backup.knowledge_figure_backups = knlge_figs

    # === save
    knowledge_backup.save
    
    # === �o�[�W�����ԍ������ăo�b�N�A�b�v�t�@�C��(.knlge)��ۑ�����
    if type == "edit" # �㏑��
      # * knowledge_backups Table.
      knowledge_backup_hash = {"gfdnavi_knowledge" => self.get_contents}

      # knowledge_figure_backups �e�[�u��
      knowledge_backup.knowledge_figure_backups.each do |kf|
        knowledge_backup_hash["gfdnavi_knowledge"]["knowledge_figures"].push({"caption"=>kf.caption, "image_path"=>kf.image_path})
      end
      File.open(self.fname[0..-7] + "." + new_version_number.to_s + ".knlge","w"){|file| file.print knowledge_backup_hash.to_yaml}
    end
  end
  
=begin
  # == �m�������̕ۑ�
#  def save(type=nil)
  def save_hogehoge(type=nil)
    # type �̃o���G�[�V����:
    #   new, new_from_analysis, temporary_save,
    #   swap, delete_figure, edit_save, edit_save_as, directory_scan, comment

    Knowledge.transaction do
      KnowledgeFigure.transaction do 
        #    ActiveRecord::Base::transaction do
        check_path(self.path) unless self.comment_on || !type || (type == "comment") || (type == "edit_save") || (type == "edit_save_as")

        # ���ɓ���path��������������ꍇ�A��������}������Ă��� (�R�}���h���C�������edit��)
        # �R�}���h���C�������edit���ɂ́Asave �̈��� edit �͎w�肵�Ȃ��̂ł���ő��v
        unless type=="swap" || type=="delete_figure" || type=="edit_save" || type=="directory_scan"
          if (k = Knowledge.find(:first, :conditions => ["path=?", path]))
            k.figures.reverse.each do |fig|
              @@to_saved_figures.unshift(fig["image"])
              @@to_saved_figures_caption.unshift(fig["caption"])
            end
          end
        end

        # === knowledge �� parent �����߂�
        FileUtils.makedirs(File.join(RAILS_ROOT, File.dirname(self.path)))
        parent = Directory.find(:first, :conditions => ["path=?",File.dirname(self.path)])
        unless parent
          parent = Directory.new
          parent.path = File.dirname(self.path)
          parent.name = File.basename(self.path)
          parent.owner = self.owner
          parent.save!
        end

        # === self.path ���g���āA�}�ƂȂ�摜���i�[����f�B���N�g���̖��O�����߂�.
        fig_dir_path = File.join(parent.path, "." + File.basename(self.path, ".knlge"))

        # ��������̓e�[�u���̒��g�̐ݒ�
        # (�f�B���N�g���X�L�������͊��ɒ��g�������Ă���̂ŕs�v)
        unless type=="directory_scan"
          # === knowledge_figures �e�[�u���̒��g��ݒ�
          knlge_figs = Array.new

          # �㏑���ۑ��̂Ƃ��A�����̉摜�͏������A�V���ɓ��꒼��
          # (���������摜�̃R�s�[�͊��� @@to_saved_figures_caption �ɓ����Ă���)
          if type == "edit_save"
            kfs = KnowledgeFigure.find(:all, :conditions => ["knowledge_id = ?", self.id])
            kas = keyword_attributes = KeywordAttribute.find(:all, :conditions => ["node_id = ?", self.node])
            kfs.each {|kf| kf.destroy }
            kas.each {|ka| ka.destroy }
          end

          @@to_saved_figures.each_with_index do |figure, i|
            #  ==== �摜��path (image_path)���擾����
            if String === figure
              # * �����̉摜��p����ꍇ�� figure �����̂܂� path
              image_path = figure
            else
              # figure �� String����Ȃ������� GfdnaviData�̂͂�
              # if NumRu::GfdnaviData === figure
              # * �摜�𕶏��Ƌ��ɕۑ�����ꍇ, path �������Ō��߂�
              if i < 10
                prefix = "image00"
              elsif i < 100
                prefix = "image0"
              else
                prefix = "image"
              end
              image_path = File.join(fig_dir_path, prefix + i.to_s + ".png")

              # �쐬�҂��X�[�p�[���[�U����Ȃ��ꍇ�A
              # save_as ���\�b�h(lib/virtual_data.rb) �ɓn�����߂� /usr/���[�U�� ����菜��
              unless self.owner.super_user?
                /^\/usr\/(.+?)(\/.+)$/ =~ image_path
                for_db_path = $2
              else
                for_db_path = image_path
              end

              figure.save_as(for_db_path, self.owner)
            end

            knlge_fig = KnowledgeFigure.new
            knlge_fig.knowledge = self # knowledges�e�[�u���� id �Ŋ֘A�t����
            knlge_fig.image_path = image_path
            knlge_fig.caption = @@to_saved_figures_caption[i]
            knlge_figs.push(knlge_fig)
          end

          # === knowledges �e�[�u���̒��g��ݒ�
          self.knowledge_figures = knlge_figs # knowledge_figures�e�[�u���� id �Ŋ֘A�t����
          self.horizontal_figures = 1 unless horizontal_figures
          self.figures_size_height_or_width = 0 unless figures_size_height_or_width
          self.figures_size_units = 0 unless figures_size_units
          self.figures_size_number = 100 unless figures_size_number
          self.default_layout = 0 unless default_layout
        end # unless type=="directory_scan"

        # === node �e�[�u���̒��g��ݒ�
        self.name = self.title
        self.other_readable = 1 # true
        self.groups_readable = -1
        self.parent = parent.node
        self.mtime = Time.new

        # === �m�������̕����R�[�h�� UTF-8 �ɕϊ�����
        #     (�{���A�\��A���ҁA�v��A�}�̃L���v�V����)
        self.textbody    = NKF.nkf('-w', self.textbody)
        self.title       = NKF.nkf('-w', self.title)
        self.creator     = NKF.nkf('-w', self.creator)
        self.description = NKF.nkf('-w', self.description)
        self.knowledge_figures.each do |fig|
          fig.caption    = NKF.nkf('-w', fig.caption)
        end

        # �����܂Ńe�[�u���ւ̒��g�̑��

        # === save �Ƃ��̌�̏���
#        unless super # Knowledge.save
        unless self.save
          # raise "failed to save a document (in knowledge.rb): #{self.errors.inspect}"
          return false
        else
          # ==== �p�����[�^������
          @@to_saved_figures = []
          @@to_saved_figures_caption = []

          # ==== node_relations table
          # + knowledge��node_id���K�v�Ȃ̂ŁA����͒m�������̕ۑ���ɍs���K�v������B
          # + �L�^����֌W
          #   * reference: ���l�f�[�^, referenced_by: �m������, name: "based_on"
          #   * reference: �摜, referenced_by: �m������, name: "based_on"
          # + Save(�㏑��)�̂Ƃ��̂݁A�O�� node_relations ������
          if type == "edit_save"
            self.delete_node_relations
          end

          # + node_relations �e�[�u���Ɋ֌W���L�^����
          # ++ KnowledgeFigure���g���āA�摜�ƕϐ��̔z������ꂼ��擾
          variables = Array.new
          images = Array.new
          self.knowledge_figures.each do |kf|
            if (img = Image.find(:first, :conditions=>["path=?", kf.image_path], :user => @user))
              images << img # �摜�̔z��
              # Gfdnavi���Ő������ꂽ�摜�Ȃ�΂�����true�ɂȂ�(�O���̉摜��org_path�������Ȃ�)
              if img.org_path
                vd = NumRu::GfdnaviData.open(img.org_path).get_object
                variables += vd.virtual_nodes # �摜�̊�ɂȂ����ϐ��̔z��
              end
            end
          end

          # ++ ��Ŏ擾�����ϐ��Ɖ摜�� node_relations�e�[�u���ɓ����
          node_relations = Array.new
          (images + variables).uniq.each do |var|
            node_relation = NodeRelation.new
            node_relation.attributes = {:reference => var.node, :referenced_by => self.node, :name => "based_on"}
            node_relations << node_relation
            node_relation.save
          end

          unless type=="directory_scan"
            # ==== .knlge �t�@�C���̍쐬(�f�B���N�g���X�L�������͏㏑�����Ȃ�)
            # * knowledge �e�[�u��
            knowledge_hash = {"gfdnavi_knowledge" => self.get_contents}
            # * knowledge_figures �e�[�u��
            self.knowledge_figures.each do |kf|
              knowledge_hash["gfdnavi_knowledge"]["knowledge_figures"].push({"image_path" => kf.image_path, "caption"  => kf.caption})
            end
            # * node_relations �e�[�u��
            node_relation_hash = {"reference" => []}
            (images + variables).uniq.each do |var|
              node_relation_hash["reference"].push({"name" => "based_on", "path" => var.path})
            end
            knowledge_hash["gfdnavi_knowledge"].merge!(node_relation_hash) if node_relation_hash

            # + .knlge�t�@�C���ւ̏�������
            File.open(self.fname,"w"){|file| file.print knowledge_hash.to_yaml}
          end # end of unless type=="directory_scan"
          return true
        end # end of unless super
      end # end of KnowledgeFigure.transaction
    end # end of Knowledge.transaction
    #    end # end of transaction
  end
=end

  # �m�������̕ʖ��ۑ�
  def save_as(path, user_name)
    knowledge_hash = self.get_contents
    knowledge_hash.delete("knowledge_figures")
    knowledge_hash.delete("owner")
    new_gd = NumRu::GfdnaviData.new(path, user_name, "knlge", knowledge_hash)
    new_gd.insert_figures = self.figures.push(user_name)
    new_gd.save("edit_save_as")
  end

  # == self �Ɋւ�� NodeRelation ������
  def delete_node_relations
    old_node_relations = NodeRelation.find(:all, :conditions => ["name=? AND referenced_by=?", "based_on", self.node_id])
    old_node_relations.each do |relation|
      relation.destroy
    end
  end

  # == (�m�������̂ЂƂ��̃f�B���N�g����)Image ��ۑ�����
  #    �ۑ����ꂽ Image �I�u�W�F�N�g��Ԃ�
  def image_save(path, index, name, img_local, user)

    # ==== �ۑ�����摜�̃p�X�����߂�
    # + path ����f�B���N�g�������߂�
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

    # + name ����t�@�C���������߂�
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
    # + �������ăp�X�����
    img_path = File.join(img_dirname, img_name)

    # ==== �摜��ۑ�����(DB�ɂ��f�B�X�N�ɂ�)
    # �쐬�҂��X�[�p�[���[�U����Ȃ��ꍇ�A
    # save_as (lib/virtual_data.rb��) �ɓn�����߂� /usr/���[�U�� ����菜��
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

  # == parent directory ��T���B
  # / ���珇�ɑS�Ē��ׁA������΍��B(DB�̂݁A���̂͂��̒��ł͕ۑ����Ȃ�)
  # ���[�v������A�Ō�̃f�B���N�g���� parent �Ƃ��� return.
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

      # = �G�̃p�X�Ƒ傫��
      #   * �G�̃p�X��nodes�e�[�u����path���擾
      #   * �G�̑傫����images�e�[�u����vizshot���擾
      image_paths = Array.new
      image_widths = Array.new
      image_heights = Array.new
      caption_widths = Array.new
      caption_heights = Array.new
      knowledge_figures.each do |kf|
        unless (image = Node.find(:first, :conditions => ["path = ?", kf.image_path], :user => user))
          #raise "Figure is not found.\n"

          # vizshot�������Ƃ���normal�T�C�Y��K�p����
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
              # vizshot���畜���ł��Ȃ������Ƃ���normal�T�C�Y��K�p����
              image_widths << 554
              image_heights << 554
              caption_widths << 440
              caption_heights << 440
            end
          rescue
=end
            # vizshot�������Ƃ���normal�T�C�Y��K�p����
            image_widths << 250
            image_heights << 254
            caption_widths << 440
            caption_heights << 440
#          end
        end
      end
      return knowledge, knowledge_figures, comments, image_paths, image_widths, image_heights, caption_widths, caption_heights
    end
    
    # OpenID�̐ړ��ɂ� http:// �� https:// �A�܂������ɂ� / ����菜��
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



