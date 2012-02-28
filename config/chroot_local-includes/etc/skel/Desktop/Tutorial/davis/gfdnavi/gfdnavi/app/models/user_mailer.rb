class UserMailer < ActionMailer::Base

  def signup_confirm(user, url)
    @subject    = '[gfdnavi] sign up confirmation'
    @body       = {:user => user, :url => url}
    @recipients = "#{user.full_name} <#{user.email_address}>"
    @from       = "#{GFDNAVI_ADMIN_NAME} <#{GFDNAVI_ADMIN_EMAIL}>"
    @sent_on    = Time.now
    @headers    = {}
  end

  def signup_inform(user, url)
    @subject    = '[gfdnavi] sign up information'
    @body       = {:user => user, :url => url}
    @recipients = User.find(:all, :conditions => ["super_user=?", true]).collect{|u| u.email_address}
    @from       = "#{GFDNAVI_ADMIN_NAME} <#{GFDNAVI_ADMIN_EMAIL}>"
    @sent_on    = Time.now
    @headers    = {}
  end

  def signup_accepted(user, url)
    @subject    = '[gfdnavi] Your signup request was accepted'
    @body       = {:user => user, :url => url}
    @recipients = "#{user.full_name} <#{user.email_address}>"
    @from       = "#{GFDNAVI_ADMIN_NAME} <#{GFDNAVI_ADMIN_EMAIL}>"
    @sent_on    = Time.now
    @headers    = {}
  end

  def signup_rejected(user, url)
    @subject    = '[gfdnavi] Your signup request was rejected'
    @body       = {:user => user, :url => url}
    @recipients = "#{user.full_name} <#{user.email_address}>"
    @from       = "#{GFDNAVI_ADMIN_NAME} <#{GFDNAVI_ADMIN_EMAIL}>"
    @sent_on    = Time.now
    @headers    = {}
  end

  def authorization_inform(user, url, suser, result)
    @subject    = '[gfdnavi] authorization information'
    @body       = {:user => user, :url => url, :suser => suser, :result => result}
    @recipients = User.find(:all, :conditions => ["super_user=?", true]).collect{|u| u.email_address}
    @from       = "#{GFDNAVI_ADMIN_NAME} <#{GFDNAVI_ADMIN_EMAIL}>"
    @sent_on    = Time.now
    @headers    = {}
  end

end
