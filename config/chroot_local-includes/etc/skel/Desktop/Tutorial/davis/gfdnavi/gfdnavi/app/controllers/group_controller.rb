class GroupController < ApplicationController
  layout  'gfdnavi'

  before_filter :login_required

  def create
    user = (login=session[:user]) && User.find_by_login(login)
    raise("only super user can create groups") if !User.super_user?(user)

    case request.method
    when :post
      if user 
        group = Group.new(:name => params[:group][:name])
        group.owner = user
        if group.save
          flash[:notice]  = "Create group successful"
          redirect_back_or_default :action => "list"
          return
        end
      end
      flash[:notice]  = "Failed to create a group"
      redirect_back_or_default :action => "list"
    end
  end

  def delete
    user = (login=session[:user]) && User.find_by_login(login)
    name = params[:id]
    group = Group.find(:first,:conditions=>["name=?",name])
    if group
      if group.owner == user || User.super_user?(user)
        if group.destroy
          flash[:notice] = "delete group successed #{name}"
        end
      end
    end
    redirect_back_or_default :action => "list"
  end

  def add_member
    case request.method
    when :post
      if ( group = Group.find(:first,:conditions=>["name=?",params[:id]]) )
        user = (login=session[:user]) && User.find_by_login(login)
        if user.super_user || group.owner == user
          unames = params[:user].split(/[;, ]+/)
          res, mesg = group.add_members(unames)
          flash[:notice] = mesg
        end
      end
      redirect_back_or_default :action => "list"
    when :get
      @group = Group.find(:first,:conditions=>["name=?",params[:id]])
    end
  end

  def delete_member
    case request.method
    when :post
      if ( group = Group.find(:first,:conditions=>["name=?",params[:id]]) )
        user = (login=session[:user]) && User.find_by_login(login)
        if user.super_user || group.owner == user
          res, mesg = group.del_member(params[:user])
          flash[:notice] = mesg
        end
      end
      redirect_back_or_default :action => "list"
    when :get
      if (group = Group.find(:first,:condition=>["name=?",params[:id]]))
        @name = group.name
        @members = group.members.collect{|u| u.login}
      end
    end
  end

  def change_owner
    case request.method
    when :post
      if ( group = Group.find(:first,:conditions=>["name=?",params[:id]]) ) && ( owner = User.find(:first,:conditions=>["login=?",(oname=params[:user])]) )
        user = (login=session[:user]) && User.find_by_login(login)
        if user.super_user || group.owner == user
          group.owner = owner
          if group.respond_to?(:update) ? group.update : group.save
            flash[:notice] = "owner was changed to #{oname}"
          else
            flash[:notice] = "faild to change owner"
          end
        end
      end
      redirect_back_or_default :action => "list"
    when :get
      if (group = Group.find(:first,:conditions=>["name=?",params[:id]]))
        @name = group.name
        members = group.members
        root = User.find_by_sql("SELECT * FROM users WHERE login='root' LIMIT 1")[0]
        members.push(root) unless members.include?(root)
        members.delete(group.owner)
        @members = members.collect{|u| u.login}
      end
    end
  end

  def list
    user = (login=session[:user]) && User.find_by_login(login)
    if User.super_user?(user)
      @own_groups = Group.find(:all)
      @super_user = true
    else
      @own_groups = user.own_groups(true)
      @super_user = false
    end
    @belonging_groups = user.belonging_groups
  end

end
