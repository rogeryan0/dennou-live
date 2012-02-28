require File.dirname(__FILE__) + '/../test_helper'

# Raise errors beyond the default web-based presentation
class UserController; def rescue_action(e) raise e end; end

class UserControllerTest < ActionController::TestCase
  fixtures :users

  def test_auth_bob
    @request.session['return-to'] = "/bogus/location"

    post :login, "user" => {"login" => "bob", "password" => "bobbob"}
    assert @response.has_session_object?("user")

    assert_equal users(:bob).login, @response.session["user"]

    assert_redirected_to "/bogus/location"
  end

  def test_signup
    post :signup, "sign_up_user" => {
      "login" => "newbob",
      "password" => "newpassword",
      "password_confirmation" => "newpassword",
      "full_name" => "Bob2",
      "email_address" => "bob@hoge.com",
      "affiliation" => "Bob corp."}
    assert !@response.has_session_object?("user")
    assert_template :signup_succeeded
  end

  def test_bad_signup
    # SEE ALSO test/units/sign_up_user_test.rb FOR BAD SIGN-UP

    # PASSWORD CONFIRMATION ERROR
    post :signup, "user" => {
      "login" => "newbob",
      "password" => "newpassword",
      "password_confirmation" => "wrong",
      "full_name" => "Bob2",
      "email_address" => "bob@bob.com",
      "affiliation" => "Bobs"}
    assert !SignUpUser.find_by_login("newbob")
    assert_template :signup
  end

  def test_invalid_login
    post :login, "user" => {"login" => "bob", "password" => "not_correct"}

    assert !@response.has_session_object?(:user)

#    assert_template_has "message"
    assert @response.has_template_object?("login")
  end

  def test_login_logoff

    post :login, :user => {"login" => "bob", "password" => "bobbob"}
    assert @response.has_session_object?(:user)

    get :logout
    assert !@response.has_session_object?(:user)

  end

  def test_set_unset_super_user
    post :login, :user => {"login" => "root", "password" => "rootroot"}
    assert !User.find_by_login(users(:bob).login).super_user?
    post :set_super_user, :name => users(:bob).login
    assert User.find_by_login(users(:bob).login).super_user?
    post :unset_super_user, :name => users(:bob).login
    assert !User.find_by_login(users(:bob).login).super_user?
  end

  def test_accept_signup
    post :signup, "sign_up_user" => {
      "login" => "newbob",
      "password" => "newpassword",
      "password_confirmation" => "newpassword",
      "full_name" => "Bob2",
      "email_address" => "bob@hoge.com",
      "affiliation" => "Bob corp."}
    sign_up_user_id = SignUpUser.find_by_login("newbob")

    post :login, :user => {"login" => "root", "password" => "rootroot"}
    post :accept_signup, "id" => sign_up_user_id
    assert User.find_by_login("newbob")
    assert_redirected_to "/user"
  end

  def test_reject_signup
    post :signup, "sign_up_user" => {
      "login" => "newbob",
      "password" => "newpassword",
      "password_confirmation" => "newpassword",
      "full_name" => "Bob2",
      "email_address" => "bob@hoge.com",
      "affiliation" => "Bob corp."}
    sign_up_user_id = SignUpUser.find_by_login("newbob")

    post :login, :user => {"login" => "root", "password" => "rootroot"}
    post :reject_signup, "id" => sign_up_user_id
    assert !User.find_by_login("newbob")
    assert_redirected_to "/user"
  end

end
