# -*- coding: cp932 -*-
class UserController < ApplicationController
  # for OpenID
  require 'ostruct'
  
  # === Proxyを使用してOpenID認証サーバと通信するために必要
  OpenID.fetcher_use_env_http_proxy if GFDNAVI_USING_PROXY

  layout  'gfdnavi'

  before_filter :login_required, :except => [:login, :signup, :openid_signup, :openid_login, :openid_logout, :authenticate, :root_url]

  def index
    @user = User.find_by_login(session[:user])
  end

  def login

    case request.method
    when :post
      user = User.authenticate(params[:user][:login], params[:user][:password])
      if user
=begin
!!! have to reset session, but this is not valide under rails 1.2
        data = session.instance_variable_get(:@data)
        data ||= session.instance_variable_get(:@attributes)
        request.reset_session
        for key, val in data
          session[key] = val
        end
=end
        session[:user] = user.login
        flash[:notice]  = "Login successful"
        redirect_back_or_default :action => "index"
      else
        flash.now[:notice]  = "Login unsuccessful"
        @login = params[:user][:login]
      end
    end
  end

  def add_user
    user = (login=session[:user]) && User.find_by_login(login)
    unless user && User.super_user?(user)
      redirect_back_or_default :action => "index"
    end
    case request.method
    when :post
      @user = User.new(params['user'])
      @user.internal_user = true
      if @user.save
        flash[:notice]  = "Signup successful"
        redirect_back_or_default :action => "index"
      end
    when :get
      @user = User.new
    end
  end

  def signup
    logout
    case request.method
    when :post
      @sign_up_user = SignUpUser.new(params['sign_up_user'])
      if @sign_up_user.save
        if GFDNAVI_ENABLE_EMAIL
          UserMailer.deliver_signup_confirm(@sign_up_user, uri_prefix)
          UserMailer.deliver_signup_inform(@sign_up_user, uri_prefix)
        end
        render :action => "signup_succeeded"
        return
      end
    when :get
      @sign_up_user = SignUpUser.new
    end
  end

  def change_password
    @user = (login=session[:user]) && User.find_by_login(login)
    if @user && @user.super_user?
      if name = params[:name]
        if user = User.find(:first,:conditions=>["login=?",name])
          @user = user
        else
          flash[:notice] = "user name is invalid"
          return
        end
      end
      @super = true
    end
    case request.method
    when :post
      password = params['current_password']
      if @super || User.authenticate(@user.login, password) == @user
        if @user.change_password(params['new_password'], params['new_password_confirmation'])
          flash[:notice]  = "Change successful"
          redirect_to :action => "index"
          return
        end
      end
      flash[:notice]  = "Change unsuccessful"
    end
  end

  def edit
    user_login = (login=session[:user]) && User.find_by_login(login)
    @user = user_login
    if @user && @user.super_user?
      if (name=params[:name]) && (user=User.find(:first,:conditions=>["login=?",name]))
        @user = user
      end
      @super = true
    end
    case request.method
    when :post
      password = params[:password]
      if user_login.super_user? || User.authenticate(@user.login, password) == @user
        @user.update_attributes(params[:user])
        if @user.save
          flash[:notice] = "Change successful"
          redirect_to :action => "index"
          return
        end
      end
      flash[:notice]  = "Change unsuccessful"
    end
  end

  def delete
    if (name = params[:name]) and (user = (login=session[:user])&&User.find_by_login(login))
      user_to_delete = User.find(:first, :conditions=>["login = ?",name])
      if user.super_user? || user == user_to_delete
        user_to_delete.destroy
      end
    end
    redirect_to :action => "index"
  end

  def logout
    session[:user] = nil
    request.reset_session
    session = request.session
  end

  def set_super_user
    if (name = params[:name]) and User.find_by_login(session[:user]).super_user
      user = User.find(:first, :conditions=>["login = ?",name])
      user.super_user = true
      user.save!
    end
    redirect_to :action => "index"
  end

  def unset_super_user
    if (name = params[:name]) and (login=session[:user]) && (user=User.find_by_login(login)) && user.super_user?
      user = User.find(:first,:conditions=>["login=?",name])
      user.super_user = false
      user.save!
    end
    redirect_to :action => "index"
  end

  def accept_signup
    suser = (login=session[:user]) && User.find_by_login(login)
    unless User.super_user?(suser)
      redirect_to :action => "index"
    end
    user = SignUpUser.find(params['id'])
    new_user = User.new
    %w(login full_name email_address affiliation).each{|attr|
      new_user.send("#{attr}=", user.send(attr))
    }
    new_user.password = "dummy"
    new_user.password_confirmation = "dummy"
    if new_user.save
      new_user.password = user.password
      new_user.save!
      user.destroy
      flash[:notice] = "authorization of #{user.login} was succeeded"
      @uri = uri_prefix
      if GFDNAVI_ENABLE_EMAIL
        UserMailer.deliver_signup_accepted(new_user, uri_prefix)
        UserMailer.deliver_authorization_inform(new_user, uri_prefix, suser, "accept")
      end
    else
      flash[:notice] = "authorization of #{user.login} was failed"
    end
    redirect_to :action => "index"
  end

  def reject_signup
    suser = (user=session[:user]) && User.find_by_login(user)
    unless User.super_user?(suser)
      redirect_to :action => "index"
    end
    user = SignUpUser.find(params['id'])
    @uri = uri_prefix
    if GFDNAVI_ENABLE_EMAIL
      UserMailer.deliver_signup_rejected(user, uri_prefix)
      UserMailer.deliver_authorization_inform(user, uri_prefix, suser, "reject")
    end
    user.destroy
    redirect_to :action => "index"
  end

  # == for OpenID
  # * gfdnavi.ymlで allow_any_openid を設定。 false なら限られたOpenIDユーザにしか許可しないモードになる。
  # * params[:open_id_complete]は、認証サーバから戻ってきたときに値が入る。それまでは nil。  
  #
  # (1) ユーザがOpenIDを入力
  # (2) using_open_idの判定で、空じゃなければ手続き続行。空なら index にリダイレクト。
  # (3) 全てのOpenIDを許すモードか、またそうでなくても登録済みのOpenIDであればauthenticate.
  #     そうでなければ、OpenID許可申請のページへと飛ばす(openid_signup へリダイレクト)。
  def openid_login
    if using_open_id?
      if GFDNAVI_ALLOW_ANY_OPENID || User.find_by_login(User.normalization(params[:openid_url]))
        authenticate
      else
        flash[:notice] = "In this Gfdnavi server, login is permitted only to admitted OpenID.\nDo you want to signup?"
        @openid_url = params[:openid_url]
        redirect_to :action => "openid_signup"
      end
    else
      flash[:notice] = "Authentication failed. Your input was empty, or wrong OpenID.\nPlease retry.\n"
      redirect_to :action => "index"
    end
  end

  def openid_logout
    session[:user_id] = nil
    redirect_to :action => "index"
  end
  
  # == OpenIDユーザの signup に使う。
  # このメソッドが呼ばれたときは、OpenID認証サーバにはまだアクセスしていないので、
  # signupのために認証をさせる。
  # 未完成
  def openid_signup
    logout
    openid_logout
    case request.method
    when :post
      authenticate_with_open_id(params[:openid_url],:required=>[:fullname, :email]) do |result, identity_url, registration|
        unless result.successful?
          flash[:error] = result.message + "Please confirm your OpenID URL.\n"
          redirect_to :action => "index"
          return
        else
          # === 認証成功
          @sign_up_openid_user = SignUpUser.new
          @sign_up_openid_user.login = identity_url
          @sign_up_openid_user.full_name = registration["fullname"]
          @sign_up_openid_user.email_address = registration["email"]
          @sign_up_openid_user.affiliation = ""
          @sign_up_openid_user.password = "dummy"
          
          if @sign_up_user.save
            if GFDNAVI_ENABLE_EMAIL
              UserMailer.deliver_signup_confirm(@sign_up_user, url)
              UserMailer.deliver_signup_inform(@sign_up_user, url)
            end
            render :action => "signup_succeeded"
            return
          end
        end
      end
    when :get
      @sign_up_user = SignUpUser.new
    end
  end
  

  protected
  # == OpenIDによるログインの認証を行う
  # * 認証の成否はここで判定している。
  def authenticate(identity_url = "")
    print "authenticate!\n"
    
    authenticate_with_open_id(params[:openid_url],:required=>[:fullname, :email]) do |result, identity_url, registration|
      # * 認証失敗なら、Userのインデックスページへとリダイレクト
      # * 認証成功なら、OpenStructオブジェクトを作り、セッションに入れる。
      unless result.successful?
        flash[:error] = result.message + "Please confirm your OpenID URL.\n"
        # redirect_to :controller => "user", :action => "index"
        jumpto = session[:jumpto] || {:controller => "user"}
      else
        openid_user = OpenStruct.new
        openid_user.identity_url = identity_url
        openid_user.fullname = registration["fullname"]
        openid_user.email = registration["email"]
        session[:user_id] = openid_user

        jumpto = session[:jumpto] || {:controller => "user"}
        session[:jumpto] = nil
        
        # 入力されたOpenIDがGfdnaviのデータベースにあるかどうか探す。
        # * 入力されたOpenIDが既に登録されている場合、full_nameとemail_addressを更新してログイン。
        # * OpenIDが未登録の場合、新規登録する。
        if external_user = User.find_by_login(identity_url)
          external_user.full_name = registration["fullname"]
          external_user.email_address = registration["email"]
          external_user.save
          session[:user] = external_user.login
          flash[:notice] = "authorization of #{identity_url} is succeeded"
        else
          new_user = User.new
          new_user.login = identity_url
          new_user.password = "dummy"
          new_user.password_confirmation = "dummy"
          new_user.full_name = registration["fullname"]
          new_user.email_address = registration["email"]
          new_user.affiliation = "external user"
          new_user.internal_user = false
          new_user.openid = true
          
          require 'pp'
          print "new_user = "
          pp new_user
          
          if new_user.save
            print "successfully login.\n"
            flash[:notice] = "creation of #{identity_url} user is succeeded"
            session[:user] = new_user.login
          else
            print "failed to save.\n"
            flash[:notice] = "creation of #{identity_url} user is failed"
          end
        end
      end
      redirect_to(jumpto)
    end
  end

  def root_url
    openid_url
  end

end
