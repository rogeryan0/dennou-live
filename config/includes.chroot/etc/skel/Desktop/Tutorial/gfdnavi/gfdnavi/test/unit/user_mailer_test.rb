require File.dirname(__FILE__) + '/../test_helper'
require 'user_mailer'

class UserMailerTest < ActiveSupport::TestCase
  FIXTURES_PATH = File.dirname(__FILE__) + '/../fixtures'
  CHARSET = "utf-8"

  include ActionMailer::Quoting

  fixtures :users, :sign_up_users

  def setup
    ActionMailer::Base.delivery_method = :test
    ActionMailer::Base.perform_deliveries = true
    ActionMailer::Base.deliveries = []

    @expected = TMail::Mail.new
    @expected.set_content_type "text", "plain", { "charset" => CHARSET }
    @expected.from = "#{GFDNAVI_ADMIN_NAME} <#{GFDNAVI_ADMIN_EMAIL}>"
 
    @user = SignUpUser.find(1)
    @suser = User.find(1)
    @url = "http://example.com"
  end

  def test_signup_confirm
    @expected.to = "#{@user.full_name} <#{@user.email_address}>"
    @expected.subject = '[gfdnavi] sign up confirmation'
    @expected.body    = read_fixture('signup_confirm')
    @expected.date    = Time.now

    email = UserMailer.create_signup_confirm(@user, @url)
    assert_equal @expected.subject, email.subject
    assert_equal @expected.to, email.to
    assert_equal @expected.from, email.from
    assert_match /Dave/, email.body
  end

  def test_signup_inform
    @expected.to = User.find(:all, :conditions => ["super_user=?",true]).collect{|u| u.email_address}
    @expected.subject = '[gfdnavi] sign up information'
    @expected.body    = read_fixture('signup_inform')
    @expected.date    = Time.now

    email = UserMailer.create_signup_inform(@user, @url)
    assert_equal @expected.subject, email.subject
    assert_equal @expected.to, email.to
    assert_equal @expected.from, email.from
    assert_match /Dave/, email.body
  end

  def test_signup_accepted
    @expected.to = "#{@user.full_name} <#{@user.email_address}>"
    @expected.subject = '[gfdnavi] Your signup request was accepted'
    @expected.body    = read_fixture('signup_accepted')
    @expected.date    = Time.now

    email = UserMailer.create_signup_accepted(@user, @url)
    assert_equal @expected.subject, email.subject
    assert_equal @expected.to, email.to
    assert_equal @expected.from, email.from
    assert_match /Dave/, email.body
    assert_match /accepted/, email.body
  end

  def test_signup_rejected
    @expected.to = "#{@user.full_name} <#{@user.email_address}>"
    @expected.subject = '[gfdnavi] Your signup request was rejected'
    @expected.body    = read_fixture('signup_rejected')
    @expected.date    = Time.now

    email = UserMailer.create_signup_rejected(@user, @url)
    assert_equal @expected.subject, email.subject
    assert_equal @expected.to, email.to
    assert_equal @expected.from, email.from
    assert_match /Dave/, email.body
    assert_match /rejected/, email.body
  end

  def test_authorization_inform
    @expected.to = User.find(:all, :conditions => ["super_user=?",true]).collect{|u| u.email_address}
    @expected.subject = '[gfdnavi] authorization information'
    @expected.body    = read_fixture('authorization_inform')
    @expected.date    = Time.now

    email = UserMailer.create_authorization_inform(@user, @url, @suser, "accept")
    assert_equal @expected.subject, email.subject
    assert_equal @expected.to, email.to
    assert_match /Dave/, email.body
    assert_match /accept/, email.body
  end

  private
    def read_fixture(action)
      IO.readlines("#{FIXTURES_PATH}/user_mailer/#{action}")
    end

    def encode(subject)
      quoted_printable(subject, CHARSET)
    end
end
