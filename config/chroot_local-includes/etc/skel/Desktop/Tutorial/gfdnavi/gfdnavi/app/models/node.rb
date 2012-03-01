require "activerecord_gfdnavi"

class Node < ActiveRecord::Base
  acts_as_tree :order => "name"

  belongs_to :owner, :class_name => "User", :foreign_key => :owner_id
  belongs_to :guest_owner, :class_name => "User", :foreign_key => :guest_owner_id

  has_many :keyword_attributes, :dependent => :destroy
  has_many :spatial_and_time_attributes, :dependent => :destroy
  has_many :draw_parameters, :dependent => :destroy

  has_many :references, :through => :node_relations_reference
  has_many :referenced_by, :through => :node_relations_referenced_by
  has_many :node_relations_reference, :class_name => "NodeRelation", :foreign_key => :referenced_by
  has_many :node_relations_referenced_by, :class_name => "NodeRelation", :foreign_key => :reference

  has_many :ancestor, :through => :node_lineages_ancestor
  has_many :descendant, :through => :node_lineages_descendant
  has_many :node_lineages_ancestor, :class_name => "NodeLineage", :foreign_key => :ancestor
  has_many :node_lineages_descendant, :class_name => "NodeLineage", :foreign_key => :descendant

  validates_presence_of  :path, :name
  validates_uniqueness_of :path

  before_create :set_default_values, :update_permission
  before_update :update_permission
  after_update  :update_children
  after_create :save_yaml, :set_nodelineage_to_ancestors, :update_parent
  before_destroy :destroy_children, :destroy_entity, :destroy_nodelineage
  after_destroy :update_parent


  DIRECTORY = 0
  VARIABLE = 1
  IMAGE = 2
  KNOWLEDGE = 3
  FUNCTION = 4
  DRAW_METHOD = 5

  NODE_TYPES = %w(directory variable image knowledge function draw_method)

  LOCAL_DRIVE = 100
  OPENDAP = 101

  RESERVED_NAMES = %w(children parent edit analysis draw)


  #<< class methods >>

  class << self

    alias :_find :find
    def find(*args)
      args.push(Hash.new) unless Hash === args[-1]
      hash = args[-1]
      user = hash.delete(:user)
      conditions = conditions_to_read(user) and \
          hash[:conditions] = add_conditions(hash[:conditions], conditions)
      _find(*args)
    end

    def count(*args)
      args.push(Hash.new) unless Hash === args[-1]
      hash = args[-1]
      user = hash.delete(:user)
      conditions = conditions_to_read(user) and \
          hash[:conditions] = add_conditions(hash[:conditions], conditions)
      super(*args)
    end

    def top_directory_nodes
      nodes = Node.find(:all, :conditions => "parent_id is NULL AND node_type=#{Node::DIRECTORY}", :order => 'path')
      if nodes.length == 0
        nodes = Node.find(:all, :conditions => "parent_id=0 AND node_type=#{Node::DIRECTORY}", :order => 'path')
      end
      return nodes
    end

    # size [Integer] size in bytes or nil
    def size2str(size)
      if size.nil?
        ''
      elsif size < 1000
        size.to_s
      elsif size < 10000
        (size / 100.0).round.to_s.insert(1,'.') + 'K'
      elsif size < 1000000
        (size / 1000).to_s + 'K'
      elsif size < 10000000
        (size / 1e5).round.to_s.insert(1,'.') + 'M'
      elsif size < 1000000000
        (size / 1000000).to_s + 'M'
      elsif size < 1e10
        (size / 1e8).round.to_s.insert(1,'.') + 'G'
      else
        (size / 1000000000).to_s + 'G'
      end
    end

    def make_user_directory(path, user, other_mode=0, rgroups=0)
      /(\/usr)\/(.+)/ =~ path or 
             raise(ArgumentError, "#{path} cannot be a user directory")
      full_path = $1    # initially /usr
      reldir = $2
      parent = Node.find(:first,:conditions=>["path=?",full_path]) or
             raise("Cannot find #{full_path}")
      dir = nil
      reldir.split(/\//).each do |dname|
        full_path = File.join(full_path, dname)
        dir = Directory.find(:first, :conditions=>["path=?",full_path], 
                             :user=>user)
        unless dir
          dir = Directory.new
          dir.name = dname
          dir.path = full_path
          dir.parent = parent
          dir.owner = user
          dir.other_mode = other_mode
          if rgroups.is_a?(Integer)
            dir.rgroups = rgroups
          else  
            # assume an array of groups
            dir.set_rgroups(*rgroups)
          end
          FileUtils.makedirs(dir.fname) or raise("failed to makedir")
          dir.save!
        end
        parent = dir.node
      end
      dir
    end

    def add_prefix(name)
      if /^http:\/\// =~ name
        return name
      elsif /^\/usr(.*)/ =~ name
        if $1
          GFDNAVI_USER_PATH + $1
        else
          GFDNAVI_USER_PATH.dup
        end
      else
        return GFDNAVI_DATA_PATH + name
      end
    end

    def remove_prefix(path)
      if /\A#{GFDNAVI_DATA_PATH}(.+)\z/ =~ path
        return $1
      elsif /\A#{GFDNAVI_USER_PATH}(.+)\z/ =~ path
        return File.join("/usr", $1)
      else
        raise "path is invalid"
      end
    end

    # protected
    # ARGUMENTS
    # * user : a User or nil (assuming non-login case) or :all.
    #   Do not use :all, if you do not understand what this means.
    # * prefix (String) : Use this if you neede to explicitly specify the
    #   name of the node table. (Useful if you use find_by_sql.)
    #   E.g. 'nodes.',
    def conditions_to_read(user, prefix='')
      if String === user
        user = User.find_by_login(user)
      end
      if User === user
        if user.super_user
          conditions = nil
        else
          groups = user.groups
          if groups == 0
            # the user is not a member of any group
            conditions = "( (#{prefix}owner_id = #{user.id}) OR " + boolean_condition("#{prefix}other_readable") + ")"
          else
            conditions = "( (#{prefix}owner_id = #{user.id}) OR NOT ((#{prefix}groups_readable & #{groups}) = 0) OR " + boolean_condition("#{prefix}other_readable") + ')'
          end
        end
      elsif user == :all
        conditions = nil
      elsif user.nil?
        conditions = '(' + boolean_condition("#{prefix}other_readable") + ')'
      else
        raise ArgumentError, "invalid user argument #{user.inspect}"
      end
      conditions
    end

  end  # end singleton methods

  #<< instance methods >>

  def set_rgroups(*groups)
    groups = groups[0] if groups.length==1 and groups[0].is_a?(Array)
    self.rgroups = Group.bit_mask_for(*groups)
  end
  def set_wgroups(*groups)
    groups = groups[0] if groups.length==1 and groups[0].is_a?(Array)
    self.wgroups = Group.bit_mask_for(*groups)
  end

  def visibility
    Group.find_by_bit_flag(self.rgroups).collect{|g| g.name}
  end

  alias _children children
  def children(reload=false, hash={})
    user = hash.delete(:user)
    if inc = hash.delete(:include)
      ch = Node.find(:all, :conditions=>["parent_id=?",self.id], :include=>inc, :user=>user)
    else
      ch = _children(reload,hash)
      if user
        ch = ch.find(:all, :user=>user, :include=>inc)
      end
    end
    return ch
  end

  @@parent_cache = Hash.new
  def parent
    return nil if parent_id == 0
    return @@parent_cache[parent_id] ||= parent_id && Node.find(:first,:conditions=>["id=?",parent_id],:user=>:all)
  end

  def entity
    return @entity if @entity
    NODE_TYPES.each {|typ|
      if node_type == Node.const_get(typ.upcase)
        @entity = ActiveRecord.class_eval(typ.classify)._find(:first,:conditions=>"node_id=#{self.id}")
	@entity.node = self
        return @entity
      end
    }
    return nil
  end

  def entity=(ent)
    @entity = ent
  end

  def node
    self
  end

  NODE_TYPES.each do |typ|
    type_num = Node.const_get(typ.upcase)
    pluralized_name = typ.pluralize
    eval <<-"EOF"
    def self.find_#{pluralized_name}(*args)
      hash = Hash===args[-1] ? args.pop : Hash.new
      hash[:select] = "nodes.*"
      hash[:from] = "nodes, #{pluralized_name}"
      case hash[:conditions]
      when String
        hash[:conditions].sub!(/^id\s*=/,"#{pluralized_name}.id=")
        hash[:conditions].sub!(/\sid\s*=/," #{pluralized_name}.id=")
      when Array
        hash[:conditions][0].sub!(/^id\s*=/,"#{pluralized_name}.id=")
        hash[:conditions][0].sub!(/\sid\s*=/," #{pluralized_name}.id=")
      when Hash
        if v = (hash[:conditions].delete(:id) || hash[:conditions].delete("id"))
          hash[:conditions]["#{pluralized_name}.id"] = v
        end
      end
      hash[:conditions] = add_conditions(hash[:conditions], "#{pluralized_name}.node_id=nodes.id")
      hash[:conditions] = add_conditions(hash[:conditions], "node_type=#{type_num}")
      if args[0].is_a?(Fixnum)
        hash[:conditions] = add_conditions(hash[:conditions], "#{pluralized_name}.id=\#{args[0]}")
        args[0] = :first
      end
      args.push hash
      self.find(*args)
    end
    def #{typ}?
      node_type == #{type_num}
    end
    @@#{typ}_nodes_cache = Hash.new
    def #{typ}_nodes(hash={})
      user = hash[:user]
#      refind = hash.delete(:refind)
#      opts = hash.dup
#      user = opts.delete(:user)
      user = user.login if User === user
#      h = ( @@#{typ}_nodes_cache[self.path] ||= Hash.new )
#      h = ( h[user] ||= Hash.new )
#      key = opts.sort.join(",")
#      if (!refind) && (obj=h[key])
#        return obj
#      end
      hash[:conditions] = add_conditions(hash[:conditions], "node_type=#{type_num}")
      obj = children.find(:all, hash)
#      h[key] = obj
      return obj
    end
    def has_#{typ}_nodes?(hash={})
      self.#{typ}_num > 0
    end
    @@#{typ}_node_length_cache = Hash.new
    def count_#{typ}_nodes(hash={})
      user = hash[:user]
#      refind = hash.delete(:refind)
#      opts = hash.dup
#      user = opts.delete(:user)
      user = user.login if User === user
#      h = ( @@#{typ}_node_length_cache[self.path] ||= Hash.new )
#      h = ( h[user] ||= Hash.new)
#      key = opts.sort.join(",")
#      if (!refind) && (obj=h[key])
#        return obj
#      end
      hash[:conditions] = add_conditions(hash[:conditions], "node_type=#{type_num}")
      len = children.count(:all, hash)
#      h[key] = len
      return len
    end
    @@#{pluralized_name}_cache = Hash.new
    def #{pluralized_name}(hash={})
      refind = hash[:refind]
      keys = hash.keys
      keys.delete(:user)
      user = hash[:user]
      user = user.login if User === user
      if keys.length == 0 
        h = ( @@#{pluralized_name}_cache[self.path] ||= Hash.new )
        if (obj=h[user]) && (!refind)
          return obj
        end
      end
      obj = #{typ}_nodes(hash).collect{|node| node.entity}
      h[user] = obj if keys.length == 0
      return obj
    end
    EOF
  end

  def fname
    if self.file && self.file!="NULL"
      return add_prefix(self.file)
    else
      return add_prefix(path)
    end
  end

  def remote?
    /^http:\/\// =~ fname
  end

  def opendap?
    remote?  # equivalent at this moment
  end

  def target
    # for bug of rails
    self
  end

  def add_prefix(name)
    if /^temporary:(.*)/ =~ name
      name = $1
      if variable? || image?
        return GFDNAVI_WORK_PATH + name
      else
        raise "[bug]"
      end
    else
      Node.add_prefix(name)
    end
  end

  def to_xml(opts={}, &b)
    entity.to_xml(opts, &b)
  end

  def to_hash(opts={}, &b)
    entity.to_hash(opts, &b)
  end

  def full_path
    "localhost@#{path}"
  end

  def stdname(name)
    self.keyword_attributes.find_by_stdname(name)
  end


  protected
  def validate
    if path =~ /^temporary/
      errors.add(:path, "temporary variable cannot be saved")
    end
    unless /^\// =~ path  || /^http:\/\// =~ path 
      errors.add(:path, "path must be started with '/' or 'http://'")
    end
    if RESERVED_NAMES.include?(name)
      errors.add(:name, "#{name} cannot be used as name")
    end
  end

  def set_default_values
    unless self.parent
      ary = path.split("/")[0..-2]
      unless ary.length == 0
        if ary.length == 1
          parent_path = "/"
        else
          parent_path = ary.join("/")
        end
        (parent = Node.find(:first,:conditions=>["path=?",parent_path])) && (self.parent = parent)
      end
    end
    self.owner = self.parent.owner if !owner && self.parent
    self.mtime = Time.new if !mtime
  end
 
  # Updates the node_lineages table (with respect to self) by writing the 
  # ids of the relationship with its ancestors.
  # 
  # It is much faster than the update_nodelineage method, 
  # but it is applicable only when the nodes table is filled in the 
  # descending order in the directory tree (which should be the case).
  # 
  def set_nodelineage_to_ancestors
    parent = self
    myid = self.id
    rel_depth = 0
    NodeLineage.transaction do
      while (parent)
        nl = NodeLineage.new
        #puts "ancestor=#{r.ancestor_id}, descendant=#{r.descendant_id}"
        nl.ancestor = parent.id
        nl.descendant = myid
        nl.rel_depth = rel_depth
        nl.save
        parent = parent.parent
        rel_depth += 1
      end
    end
  end
  def destroy_nodelineage
    nls = NodeLineage.find(:all, :conditions=>["descendant=?",self.id])
    NodeLineage.transaction do
      nls.each do |nl|
        nl.destroy
      end
    end
  end

  def update_parent
    if pa = self.parent
      return pa.update_node_number(node_type)
    else
      return true
    end
  end

  def update_node_number(node_type)
    mname = NODE_TYPES[node_type]
    self.send("#{mname}_num=", self.send("count_#{mname}_nodes", :user=>:all))
    self.save
  end

  # THIS METHOD IS CURRENTLY UNUSED -- Insted,
  # set_nodelineage_to_ancestors is used. (2008/06/02 horinout)
  #
  # Updates the node_lineages table for self.
  # It is complete but takes time, since the nodes table is
  # fully scanned.
  def update_nodelineage
    NodeLineage.delete_all(["ancestor=? or descendant=?",self.id,self.id])
    qstr=<<-EOM
       select n1.id as ancestor_id, n1.path as ancestor_path,
              n2.id as descendant_id ,n2.path as descendant_path 
       from nodes n1 join nodes n2 on concat(n2.path,'/') like concat(n1.path,'/%') 
            or n1.path='/' 
       where n1.id=#{self.id} or n2.id=#{self.id};
    EOM
    res = NodeLineage.find_by_sql(qstr)
    res.each{ |r|
       nr = NodeLineage.new
       #puts "ancestor=#{r.ancestor_id}, descendant=#{r.descendant_id}"
       nr.ancestor = r.ancestor_id
       nr.descendant = r.descendant_id
       nr.rel_depth = r.descendant_path.gsub(/[^\/#]/,"").length-r.ancestor_path.gsub(/[^\/#]/,"").length
       nr.save
    }
  end

  def update_permission
    @change_permission = false
    if !parent
      # a root node
      if self.other_mode.nil? || (self.other_mode & 4)!=0  # r ok for others
        # anyone can read
        unless self.other_readable == true
          self.other_readable = true
          @change_permission = true
        end
        unless self.groups_readable == -1
          self.groups_readable = -1   # == unsigned 0xffffffffffffffff
          @change_permission = true
        end
      else
        # read limited only to permited groups
        unless self.other_readable == false
          self.other_readable = false
          @change_permission = true
        end
        unless self.groups_readable == self.rgroups
          self.groups_readable = self.rgroups 
          @change_permission = true
        end
      end
    else
      if self.other_mode.nil? || (self.other_mode & 4)!=0  # r ok for others
        # settings same as the direct parent
        unless self.other_readable == parent.other_readable
          self.other_readable = parent.other_readable
          @change_permission = true
        end
        unless self.groups_readable == parent.groups_readable
          self.groups_readable = parent.groups_readable
          @change_permission = true
        end
      else
        # read limited only to permited groups
        unless self.other_readable == false
          self.other_readable = false
          @change_permission = true
        end
        unless self.groups_readable == parent.groups_readable & self.rgroups 
          self.groups_readable = parent.groups_readable & self.rgroups
          @change_permission = true
        end
      end
    end
  end

  def update_children
    if @change_permission
      c = children(false,{:user=>:all})
      if c.length > 0
        c.each{|child|
          if child.respond_to?(:update)
            child.update
          else
            child.save
          end
        }
      end
    end
    @change_permission = nil
  end

  def destroy_children
    children(true, :user=>:all).each{|c| c.destroy}
  end

  def destroy_entity
    entity.destroy if entity && !entity.to_be_destroyed
  end

  def save_yaml
    return if knowledge? || function? || draw_method?
    return if file or remote?
    yaml_fname = fname + ".yml"
    sigen_fname = fname + ".SIGEN"
    hash = make_attribute_hash
    unless hash.length==0 || file || (variable? && entity.actual_files.length>0)
      unless File.exist?(yaml_fname) || File.exist?(sigen_fname)
        File.open(yaml_fname,"w"){|file| file.print hash.to_yaml}
      end
    end
  end


  def make_attribute_hash
    hash = Hash.new
    keyword_attributes.each{|key|
      hash[key.name] = key.value
    }
    vh = Hash.new
    variable_nodes(:user => :all).each{|var|
      vh[var.name] = var.make_attribute_hash
    }
    if vh.length > 0
      hash['contains'] = vh
    end
    chash = Node.columns_hash
    gh = Hash.new
    gh['user'] = owner.login if owner && (parent && owner != parent.owner)
    gh['other_mode'] = other_mode if other_mode && other_mode!=chash['other_mode'].default
    if (gs = Group.find_by_bit_flag(rgroups)) && gs != []
      gh['rgroups'] = gs.collect{|g| g.name}
    end
    if (gs = Group.find_by_bit_flag(wgroups)) && gs != []
      gh['wgroups'] = gs.collect{|g| g.name}
    end
    gh['guest_owner_id'] = guest_owner_id if guest_owner_id
    if references.length > 0
      gh['references'] = references.collect{|ref|
        {'path' => ref.path, 'name' => ref.name}
      }
    end
    if image? && entity.org_path
      gh['org_path'] = entity.org_path
    end
    hash['gfdnavi'] = gh if gh.length > 0
    return hash
  end


  def make_directories
    if file && file!=""
      apath = file.splite("/")[1..-2]
    else
      apath = path.splite("/")[1..-1]
    end
    dir = Node.find_by_sql("SELECT * FROM nodes WHERE path='/' LIMIT 1")[0]
    user = "root"
    if apath[0] = "usr"
      user = apath[1]
      apath = apath[2..-1]
      dir = make_directory(dir, "usr", "root")
      dir = make_directory(dir, user, user)
    end
    apath = apath.split("/")
    apath.each{|name|
      dir = make_directory(dir, name, user)
    }
  end

  def make_directory(parent, name, uname)
    user = User.find(:first,:conditions=>["login=?",uname])
    path = File.join(parent.path, name)
    node = Node.find(:first, :conditions=>["path=?",path], :usr=>user)
    if node
      return node.entity
    else
      dir = Directory.new(:name=>name, :path=>path)
      dir.user = user
      return dir
    end
  end


end
