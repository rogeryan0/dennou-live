require File.dirname(__FILE__) + '/../test_helper'

class SignUpUserTest < ActiveSupport::TestCase
  fixtures :sign_up_users

  def setup
    @user = SignUpUser.find(1)
  end

  def test_create
    dave = sign_up_users(:dave)
    assert_kind_of SignUpUser, @user
    assert_equal dave.id, @user.id
    assert_equal dave.login, @user.login
    assert_equal dave.full_name, @user.full_name
    assert_equal dave.email_address, @user.email_address
    assert_equal dave.affiliation, @user.affiliation
    assert_equal dave.password, @user.password
  end

  def test_already_exist
    su = SignUpUser.new("login" => sign_up_users(:dave).login,
                        "password" => "newpassword",
                        "password_confirmation" => "wrong",
                        "full_name" => "dave2",
                        "email_address" => "dave@dave.com",
                        "affiliation" => "dave corp.")
    assert !su.save
  end

  def test_password_confirmation_error
    su = SignUpUser.new("login" => "newbob",
                        "password" => "newpassword",
                        "password_confirmation" => "wrong",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_too_short_password
    su = SignUpUser.new("login" => "newbob",
                        "password" => "pass", # length of 4
                        "password_confirmation" => "pass",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_too_long_password
    su = SignUpUser.new("login" => "newbob",
                        "password" => "123456678901234567890123456789012345678901", # length of 41
                        "password_confirmation" => "123456678901234567890123456789012345678901",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_too_short_login_name
    su = SignUpUser.new("login" => "yo", # length of 2
                        "password" => "newpassword", 
                        "password_confirmation" => "newpassword",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_too_long_login_name
    su = SignUpUser.new("login" => "yoyoyoyoyoy", # length of 11 
                        "password" => "newpassword", 
                        "password_confirmation" => "newpassword",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_too_short_login_name_and_password_confirmation_error
    su = SignUpUser.new("login" => "yo",
                        "password" => "newpassword",
                        "password_confirmation" => "wrong",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_bad_email_address_1
    su = SignUpUser.new("login" => "newbob",
                        "password" => "newpassword",
                        "password_confirmation" => "newpassword",
                        "full_name" => "Bob2",
                        "email_address" => "hoge",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_bad_email_address_2
    su = SignUpUser.new("login" => "newbob",
                        "password" => "newpassword",
                        "password_confirmation" => "newpassword",
                        "full_name" => "Bob2",
                        "email_address" => "hoge@hoge",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_bad_email_address_3
    su = SignUpUser.new("login" => "newbob",
                        "password" => "newpassword",
                        "password_confirmation" => "newpassword",
                        "full_name" => "Bob2",
                        "email_address" => "hoge.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_missing_login_name
    su = SignUpUser.new("password" => "newpassword",
                        "password_confirmation" => "newpassword",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_missing_full_name
    su = SignUpUser.new("login" => "newbob",
                        "password" => "newpassword",
                        "password_confirmation" => "newpassword",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_missing_email_address
    su = SignUpUser.new("login" => "newbob",
                        "password" => "newpassword",
                        "password_confirmation" => "newpassword",
                        "full_name" => "Bob2",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_missing_affiliation
    su = SignUpUser.new("login" => "newbob",
                        "password" => "newpassword",
                        "password_confirmation" => "newpassword",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com")
    assert !su.save
  end

  def test_missing_password
    su = SignUpUser.new("login" => "newbob",
                        "password_confirmation" => "newpassword",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_missing_password_confirmation
    su = SignUpUser.new("login" => "newbob",
                        "password" => "newpassword",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

  def test_missing_both_password_and_password_confirmation
    su = SignUpUser.new("login" => "newbob",
                        "full_name" => "Bob2",
                        "email_address" => "bob@bob.com",
                        "affiliation" => "Bobs")
    assert !su.save
  end

end
