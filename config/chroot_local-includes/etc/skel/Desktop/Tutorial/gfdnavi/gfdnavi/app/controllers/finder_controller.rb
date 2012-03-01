#require "create_activeresources"

class FinderController < ApplicationController

  layout "gfdnavi"


  def index
    @path = params[:path]
  end

  def clear_tree
    session[:dir_tree] = nil
    session[:dir_tree_closed] = nil
    session[:dir_tree_selected] = nil
    redirect_to :action => :index
  end

  def create_tree
    if (path = params[:path])
      dir = open_tree(path)
      @selected_dir = session[:dir_tree_selected] = dir
    end
    @user = (login=session[:user]) && User.find_by_login(login)
    @parent = nil
    @dirs = Node.top_directory_nodes
    if GFDNAVI_OTHER_GFDNAVI_SERVERS
      GFDNAVI_OTHER_GFDNAVI_SERVERS.each{|gs|
      }
    end
=begin
   Node_Remote.each{|nr|
      @dirs += nr.find(:all, :from => :top_directory_nodes)
    }
=end
    @opened_dirs = (session[:dir_tree] ||= Array.new)
    @closed_dirs = (session[:dir_tree_closed] ||= Array.new)
    @selected_dir = session[:dir_tree_selected]
    if @dirs.length == 1
      path = @dirs[0].full_path
      @opened_dirs.push(path) unless @opened_dirs.include?(path)
      @closed_dirs.delete(path)
    end
    render :layout => false
  end

  def children
    user = (login=session[:user]) && User.find_by_login(login)
    unless (path = params[:path]) && (parent = find_node(path,user) )
      head :not_found
      return
    end

    unless request.xhr?
      redirect_to :action => :index
      return
    end
    
    selected = params[:selected]

    session[:dir_tree] ||= Array.new
    open_dir([], [parent], session, selected)
  end

  def add_to_list
    path = params[:path]
    add_node_to_list(path)
    redirect_to(:controller => "analysis2")
  end

  def items_selected
    button = String.new
    params.keys.each { |key|
      case key
      when /^anal_viz/
        button = "Anal/Viz checked items"
        break
      when /^show/
        button = "Show checked items"
        break
      when /^dl/
        button = "Download checked items"
        break
      end
    }
    case button
      when "Anal/Viz checked items" then anal_viz_checked_items
      when "Show checked items"     then show_checked_items
      when "Download checked items" then download_checked_items
      else                          raise "error.\n"
    end
  end

  def dir_status
    unless request.xhr?
      redirect_to :action => :index
      return
    end
    session[:dir_tree_closed] ||= Array.new
    user = (login=session[:user]) && User.find_by_login(login)
    if (path = params["opened"])
      session[:dir_tree_closed].delete path
      if params["selected"]
        dir = find_node(path, user)
        session[:dir_tree_selected] = dir
      end
    elsif (path = params["closed"])
      dir = find_node(path, user)
      if dir
        session[:dir_tree_closed].push(path) unless session[:dir_tree_closed].include?(path)
        if (dir_selected = session[:dir_tree_selected])
          dir_selected_path = dir_selected.path
          if /^#{path}/ =~ dir_selected_path && path != dir_selected_path
            render :update do |page|
              session[:dir_tree_selected] = dir
              page << "tree.dirSelected('#{path}', true);"
            end
            return
          end
        end
      end
    end
    render :nothing => true
  end

  def show_details
    if GFDNAVI_DATA_URL_PREFIX
      @url_prefix = relative_url_root + GFDNAVI_DATA_URL_PREFIX
    end
    user = (login=session[:user]) && User.find_by_login(login)
    if path = params[:path]
      dir = find_node(path,user)
    else
      dir = session[:dir_tree_selected]
    end
    if dir
      session[:dir_tree_selected] = dir
      order = params[:order]
      @user = user
      @path = dir.path
      @dirs = dir.directory_nodes(:user=>@user, :order=>order)
      @dirs.delete_if {|d| d.name =~ /^[.]/}
      @vars = dir.variable_nodes(:user=>@user, :order=>order)
      @imgs = dir.image_nodes(:user=>@user, :order=>order)
      @klgs = dir.knowledge_nodes(:user=>@user, :order=>order)
      @funcs = dir.function_nodes(:user=>@user, :order=>order)
      @dms = dir.draw_method_nodes(:user=>@user, :order=>order)
      render :layout => false
    else
      render :nothing => true
    end
  end

  def open_tree(path)
    if path
      flag = true
    else
      unless request.xhr?
        redirect_to :action => :index
        return
      end
      path = params[:path]
      flag = false
    end
    user = (login=session[:user]) && User.find_by_login(login)
    unless path && dir = find_node(path,user)
      render(:nothing => true) unless flag
      return
    end
    tree = ( session[:dir_tree] ||= Array.new )
    closed = ( session[:dir_tree_closed] ||= Array.new )
    ancestors = Array.new
    closes = Array.new unless flag
    parent = dir
    while (parent = parent.parent)
      pa = parent.full_path
      if tree.include?(pa)
        if closed.include?(pa)
          if flag
            closed.delete parent.path
          else
            closes.unshift parent
          end
        end
      else
        ancestors.unshift parent
      end
    end
    if flag
      ancestors.each{|an| tree.push an.full_path}
      return dir
    else
      closes.push(dir) if tree.include?(path) && !closed.include?(path)
      open_dir(closes, ancestors, session, false)
    end
  end

  def show_images
    if path = params[:path]
      list = [[path, "1"]]
    else
      list = params[:list]
    end
    @images = Array.new
    if list
      list.each{|path,v|
        if v == "1"
          user = (login=session[:user]) && User.find_by_login(login)
          node = Node.find(:first, :conditions => ["path=?", path], :user => user)
          @images << path if node.image?
        end
      }
    end
    render :layout => !request.xhr?
  end


  def flex
    @uri_prefix = uri_prefix.sub(/^http:\/\//,"")
    render :layout => false
  end

  protected


  def anal_viz_checked_items
    list = params[:list]
    if list
      list.each{|path,v|
        add_node_to_list(path) if v == "1"
      }
    end
    redirect_to(:controller => "analysis2")
  end

  def download_checked_items
    redirect_to :action => "index"
  end

  
  def open_dir(closes, parents, session, selected)
    dir_tree = session[:dir_tree]
    user = (login=session[:user]) && User.find_by_login(login)
    if parents[0]
      grand_parent = parents[0].parent
      if grand_parent && !dir_tree.include?(grand_parent.full_path)
        render :nothing => true
        return false
      end
    end
    render :update do |page|
      closes.each{|dir|
        page << "tree.dirChange('#{dir.full_path}');"
      }
      parents.each{|dir|
        path = dir.full_path
        @dirs = dir.directory_nodes(:user=>user)
        if !(dir_tree.include?(path)) && @dirs.length > 0
          @user = user
          @parent = dir
          session[:dir_tree].push path
          page.replace "dir#{path}", render(:partial => "children")
        end
      }
      if selected
        page << "tree.dirSelected('#{parents[-1].full_path}');"
      end
    end
    session[:dir_tree_selected] = parents[-1] if selected
  end

  def add_var_to_list(var)
    if Node === var
      var = var.entity
    elsif String === var
      user = (login=session[:user]) && User.find_by_login(login)
      var = Variable.find(:first, :conditions=>["path=?",path,], :user=>user )
    end
    if var
      session[:variables_list] ||= Array.new
      path = var.path
      session[:variables_list].push path unless session[:variables_list].include?(path)
    end
  end

  def add_node_to_list(node)
    user = (login=session[:user]) && User.find_by_login(login)
    if String === node
      begin
        node = Node.find(:first, :conditions=>["path=?",node], :user=>user)
        return nil unless node
      end
    end

    if node.variable?
      add_var_to_list(node)
      return
    elsif node.directory?
      node.variable_nodes(:user=>user).each{|var|
        add_var_to_list(var)
      }
      node.directory_nodes(:user=>user).each{|dir|
        add_dir_to_list(dir)
      }
    end
  end


  protected
  def find_node(path,user)
    if /^([^@]+)@(.+)$/ =~ path
      site = $1
      path = $2
    else
      site = "localhost"
    end
    if site == "localhost"
      return Node.find(:first, :conditions => ["path=?",path], :user => user)
    else
=begin
      Node_Remote.each{|nr|
        if nr.site.to_s == site
          return nr.find(:one, :from => "/nodes/show.xml?path=#{path}")
        end
      }
=end
    end
    return nil
  end



end
