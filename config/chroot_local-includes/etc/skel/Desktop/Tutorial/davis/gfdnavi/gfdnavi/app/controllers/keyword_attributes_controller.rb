class KeywordAttributesController < ApplicationController

  layout "gfdnavi"

  def index
    path = params[:path]
    path = "" unless path
    path = File.join("",path)
    user = (login=session[:user]) && User.find_by_login(login)
    node = Node.find(:first, :conditions => ["path=?",path], :user => user)
    if node
      @kas = node.keyword_attributes
      respond_to do |format|
        format.html
        format.xml { render :xml => @kas}
      end
    else
      respond_to do |format|
        format.html {
          render :text => "not found", :status => :not_found
        }
        format.xml { head :not_found }
      end
    end
  end

=begin
  def show
    if id = params[:id]
      user = (login=session[:user]) && User.find_by_login(login)
      ka = KeywordAttribute.find(id)
      if ka && ka.node(false,:user=>user)
        @ka = ka
        respond_to do |format|
          format.html
          format.xml {render :xml => @ka}
        end
        return
      end
    end
    respond_to do |format|
      format.html {
        flash[:notice] = "not found"
        redirect_to :status => :not_found, :controller => "finder"
      }
      format.xml { head :not_found }
    end
  end
=end


end
