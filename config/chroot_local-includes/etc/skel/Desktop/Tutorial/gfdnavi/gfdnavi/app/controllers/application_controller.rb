# Filters added to this controller will be run for all controllers in the application.
# Likewise, all the methods added will be available for all controllers.
require_dependency "login_system"
require "basic_authorization"
require "virtual_data"
require "numru/gfdnavi_data"
require "numru/gfdnavi_data/local"

class ApplicationController < ActionController::Base
  include LoginSystem
  include BasicAuthorization
  before_filter :session_expires
  
  filter_parameter_logging :password


  protected
  def session_expires
    ::ActionController::CgiRequest::DEFAULT_SESSION_OPTIONS.update(
      :session_expires => GFDNAVI_SESSION_DURATION.minute.from_now
    )
  end

  def user_path
    GFDNAVI_USER_PATH + "/" + session[:user]
  end

  def uri_prefix
    prot = request.ssl? ? "https" : "http"
    "#{prot}://#{request.host_with_port}#{relative_url_root}/"
  end

  def relative_url_root
    if request.respond_to?(:relative_url_root)
      # for < 2.2.2
      request.relative_url_root
    else
      # for >= 2.2.2
      super || ""
    end
  end

end
