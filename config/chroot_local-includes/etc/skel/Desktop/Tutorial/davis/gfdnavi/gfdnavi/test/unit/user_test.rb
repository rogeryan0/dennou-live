require File.dirname(__FILE__) + '/../test_helper'

class UserTest < ActiveSupport::TestCase
  # self.use_instantiated_fixtures  = true

  fixtures :users, :groups

  def setup
    @root = User.find(1)
    @bob = User.find(2)
    @longbob = User.find(3)
  end

  def test_auth
    assert_equal  @bob, User.authenticate("bob", "bobbob")
    assert_nil    User.authenticate("bob", "test")
    assert_nil    User.authenticate("nonbob", "test")
  end

  def test_belonging_groups
    assert_equal @root.belonging_groups.collect{|g| g.name}.sort, 
                 ["all", "root"]
  end

  def test_passwordchange
    new_password = "nonbobpasswd"
    assert ! @longbob.change_password(new_password, new_password+"dummy")
    assert @longbob.change_password(new_password, new_password)
    assert_equal @longbob, User.authenticate("longbob", "nonbobpasswd")
    assert_nil  User.authenticate("longbob", "longtest")
  end

  def test_disallowed_passwords
    u = new_user("nonbob")
    u.password = u.password_confirmation = "tiny"
    assert !u.save
    assert u.errors.invalid?('password')

    u.password = u.password_confirmation = "hugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehugehuge"
    assert !u.save
    assert u.errors.invalid?('password')

    u.password = u.password_confirmation = ""
    assert !u.save
    assert u.errors.invalid?('password')

    u.password = u.password_confirmation = "bobs_secure_password"
    assert u.save
    assert u.errors.empty?

  end

  def test_bad_logins
    u = new_user("x")
    assert !u.save
    assert u.errors.invalid?('login')

    u.login = "hugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhugebobhug"
    assert !u.save
    assert u.errors.invalid?('login')

    u.login = ""
    assert !u.save
    assert u.errors.invalid?('login')

    u.login = "oklogin"
    assert u.save
    assert u.errors.empty?

  end


  def test_uniq
    u = new_user("bob")
    assert !u.save
    assert u.errors.invalid?('login')
  end


  def test_full_name
    u = new_user("bobfullname")
    u.full_name = ""
    assert !u.save
    assert u.errors.invalid?('full_name')
  end


  def test_affiliation
    u = new_user("bobaffiliation")
    u.affiliation = ""
    assert !u.save
    assert u.errors.invalid?('affiliation')
  end


  def test_email_address
    u = new_user("bobemail")
    u.email_address = ""
    assert !u.save
    assert u.errors.invalid?('email_address')

    u.email_address = "@example.com"
    assert !u.save
    assert u.errors.invalid?('email_address')

    u.email_address = "bob@example"
    assert !u.save
    assert u.errors.invalid?('email_address')

    u.email_address = "bobexample.com"
    assert !u.save
    assert u.errors.invalid?('email_address')
  end


  def test_sha1
    u = new_user("bobsha")
    u.password = u.password_confirmation = "okpassword"
    assert u.save
    assert_equal User.sha1("okpassword"), u.password
  end

  private
  def new_user(login)
    u = User.new
    u.login = login
    u.full_name = "Non BoB"
    u.email_address = "nonbob@example.com"
    u.affiliation = "BOB coorp"
    u.password = u.password_confirmation = "nonbob"
    u
  end

end
