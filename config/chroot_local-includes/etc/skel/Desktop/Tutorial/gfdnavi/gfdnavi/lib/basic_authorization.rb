require_dependency "user"

module BasicAuthorization
  protected

  def basic_authorization
    return get_basic_user
  end

  def basic_authorization_required
    if get_basic_user
      if @basic_login
        return false
      end
      return true
    else
      return false
    end
  end

  def basic_login
    @basic_login
  end

  private
  def get_basic_user
    if src = request.env["HTTP_AUTHORIZATION"]
      type, body = src.split
      if type == "Basic"
        login, password = Base64.decode64(body).split(":")
        if login == "guest"
          @basic_login = nil
          return true
        end
        if ( user = User.authenticate(login, password) )
          @basic_login = login
          return true
        end
      end
    else
      @basic_login = nil
      return true
    end
    response.headers["WWW-Authenticate"] = 'Basic realm="guest can access without password"'
    render :text => "Atuthorization requireed", :layout => false, :status => :unauthorized
    return false
  end

end
