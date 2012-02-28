class SignUpUser < ActiveRecord::Base


  protected

  before_create :crypt_password
  def crypt_password
    write_attribute("password", User.sha1(password))
  end

  def validate
    if User.find(:first,:conditions=>["login=?",login])
      errors.add(:login, "is already being used")
    end
  end

  validates_length_of :login, :within => 3..10
  validates_length_of :password, :within => 5..40
  validates_presence_of :login, :password, :full_name, :email_address, :affiliation
  validates_presence_of :password_confirmation, :on => :create
  validates_confirmation_of :password, :on => :create
  validates_uniqueness_of :login
  validates_format_of :email_address, :with => /^(\w|-)+@(\w|-)+\.(\w|-)+/, :message => "email address is invalid"

end
