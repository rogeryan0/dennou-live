class Group < ActiveRecord::Base

  MAX_GROUP_NO = 64

  belongs_to :owner, :class_name => "User", :foreign_key => "owner_id"
#  has_many :members, :through => :group_members, :source => :user, :order => :login
#  has_many :group_members, :dependent => :destroy

  after_save :save_file
  after_destroy :save_file_destroy

  before_validation_on_create :set_id
  validates_presence_of :name, :owner_id
  validates_uniqueness_of :name, :on => :create
  validates_inclusion_of :id, :in=>1..MAX_GROUP_NO, :on => :create

  def bit_mask
    1 << (id-1)
  end

  def self.bit_mask_for(*groups)
    ids = Array.new
    groups.each do |g|
      case g
      when Integer
        id = g
        ids.push(id) if id>1 and id<MAX_GROUP_NO
      when Group
        ids.push(g.id)
      when String
        if (gr=Group.find_by_name(g))  # substitution, not ==
          ids.push(gr.id)
        end
      else
        raise ArgumentError, "Unsupported kind of object : #{g.inspect}"
      end
    end
    bitmask = 0
    ids.each{|id| bitmask |= (1 << (id-1))}
    bitmask
  end

  def members
    User.find(:all, :conditions => ["(groups & ?) != 0",bit_mask])
  end

  def self.find_by_bit_flag(groups)
    ids = Array.new
    (1..MAX_GROUP_NO).each do |i| 
      ids.push(i) if ( groups % 2 == 1 )
      groups = groups >> 1
    end
    if ids.length == 0
      []
    elsif ids.length == 1
      [self.find(ids[0])]
    else
      self.find(*ids)
    end
  end

  # * users (Array of User) : members to add
  #
  def add_members(users)
    bm = bit_mask
    User.transaction do
      users.each do |usr|
	if String === usr
          usr = User.find(:first,:conditions=>["login=?",un=usr])
          unless usr
            return [false, "No such a usr #{un}"]
          end
        end
	usr.groups = ( usr.groups | bm )
	usr.save!
      end
    end
    return [true, "add members successed"]
  end

  alias members= add_members

  def add_member(user)
    add_members([user])
  end

  def del_members(users)
    mask = ~bit_mask
    User.transaction do
      users.each do |usr|
	unless User === usr
          usr = User.find(:first,:conditions=>["login=?",un=usr])
          unless usr
            return [false, "No such a usr #{un}"]
          end
        end
	usr.groups = ( usr.groups & mask )
	usr.save!
      end
    end
    return [true, "delete member successed"]
  end

  def del_member(user)
    del_members([user])
  end

  def before_destroy
    mask = ~bit_mask
    User.update_all("groups = groups & #{mask}")
    Node.update_all("rgroups = rgroups & #{mask}")
    Node.update_all("wgroups = wgroups & #{mask}")
    true
  end

  def set_id
    min_id = ((1..MAX_GROUP_NO).to_a - Group.find(:all).collect{|g| g.id})[0]
    if min_id.nil?
      raise("Exceeded max group num (#{MAX_GROUP_NO}). Delete one or more.") 
    end
    self.id = min_id
    true
  end

  def save_file
    fname = File.join(RAILS_ROOT,"db","group_#{RAILS_ENV}.yml")
    if File.exist?(fname)
      yml = YAML.load(File.read(fname))
      unless Hash === yml
        raise "data is invalid"
      end
      backup = fname+"~"
      File.rename(fname, backup)
    else
      yml = Hash.new
    end
    hash = Hash.new
    self.class.column_names.each{|name|
      hash[name] = self[name]
    }
    yml[self.name] = hash
    File.open(fname,"w"){|file| file.print yml.to_yaml}
  end

  def save_file_destroy
    fname = File.join(RAILS_ROOT,"db","group_#{RAILS_ENV}.yml")
    if File.exist?(fname)
      yml = YAML.load(File.read(fname))
      unless Hash === yml
        raise "data is invalid"
      end
      backup = fname+"."+Time.now.strftime("%Y%m%d%H%M%S")
      raise("backup file already exists") if File.exist?(backup)
      File.rename(fname, backup)
    else
      yml = Hash.new
    end
    yml.delete(self.name)
    File.open(fname,"w"){|file| file.print yml.to_yaml}
  end


#  def create_or_update  # called in save and save!
#    Group.transaction do
#      super
#      bit_flag = 2**(id-1)
#      STDERR.puts '@@@@@@',bit_flag
#      nil
#    end
#  end

end
