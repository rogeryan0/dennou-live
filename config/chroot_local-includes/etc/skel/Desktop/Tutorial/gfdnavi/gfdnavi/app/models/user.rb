require 'digest/sha1'

# this model expects a certain database layout and its based on the name/login pattern.
class User < ActiveRecord::Base
  attr_protected :super_user
  has_many :own_groups, :class_name => "Group", :foreign_key => "owner_id", :dependent => :destroy
  #has_many :belonging_groups, :through => :group_members, :source => :group, :order => :name

  before_create :crypt_password
  after_save :save_file
  after_destroy :save_file_destroy

  def self.authenticate(login, pass)
    find(:first,
         :conditions => ["login = ? AND password = ?", login, sha1(pass)])
  end

  def self.super_user?(user)
    user.super_user?
  end

  def super_user?
    super_user == true
  end

  def change_password(pass, pass_confirm)
    unless pass == pass_confirm
      return false
    end
    self.password = User.sha1(pass)
    self.save
  end

  def belonging_groups
    Group.find_by_bit_flag(groups)
  end

  protected

  def self.sha1(pass)
    Digest::SHA1.hexdigest("#{GFDNAVI_PASSPHRASE}--#{pass}--")
  end

  def crypt_password
    write_attribute("password", self.class.sha1(password))
  end

=begin
  def after_create
    group = self.own_groups.create(:name => self.login)
    group.members = [self]
  end
=end

  def save_file
    fname = File.join(RAILS_ROOT,"db","user_#{RAILS_ENV}.yml")
    if File.exist?(fname)
      yml = YAML.load(File.read(fname))
      unless Hash === yml
        raise "data is invalid"
      end
      backup = fname + "~"
      File.rename(fname, backup)
    else
      yml = Hash.new
    end
    hash = Hash.new
    self.class.column_names.each{|name|
      hash[name] = self[name]
    }
    yml[self.login] = hash
    File.open(fname,"w"){|file| file.print yml.to_yaml}
  end

  def save_file_destroy
    fname = File.join(RAILS_ROOT,"db","user_#{RAILS_ENV}.yml")
    if File.exist?(fname)
      yml = YAML.load(File.read(fname))
      unless Hash === yml
        raise "data is invalid"
      end
      backup = "#{fname}.#{Time.now.strftime('%Y%m%d%H%M%S')}"
      raise("backup file already exists") if File.exist?(backup)
      File.rename(fname, backup)
    else
      yml = Hash.new
    end
    yml.delete(self.login)
    File.open(fname,"w"){|file| file.print yml.to_yaml}
  end


  validates_length_of :login , :within => 3..100
  validates_length_of :password, :within => 5..40
  validates_presence_of :login, :password, :full_name, :email_address, :affiliation
  validates_presence_of :password_confirmation, :on => :create
  validates_confirmation_of :password, :on => :create
  validates_uniqueness_of :login
  validates_format_of :email_address, :with => /^(\w|-)+@(\w|-)+\.(\w|-)+/, :message => "email address is invalid"


  #<< class methods >>
  class << self
    # == OpenID‚Ì³‹K‰»‚ğs‚¤
    def normalization(openid_url)
      openid_url = openid_url[6..-1] if /^xri\:\/\// =~ openid_url
      return openid_url if /^[=@!\+\$\(]/ =~ openid_url
      return openid_url if /^https\:\/\// =~ openid_url
      openid_url = "http://" + openid_url unless /^http\:\/\// =~ openid_url
      openid_url += "/" unless /^http\:\/\/.+?\/.*?$/ =~ openid_url

      return openid_url
    end
  end
end
