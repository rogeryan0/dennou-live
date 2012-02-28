class DrawMethodController < ApplicationController
  layout "gfdnavi"

  before_filter :login_required, :except => [:index, :list, :details]

  def index
    redirect_to :action => "list"
  end

  def create
    user = (login=session[:user]) && User.find_by_login(login)
    @directory = "/usr/#{user.login}/draw_methods"
    case request.method
    when :post
      @draw_method = DrawMethod.new
      @draw_method.attributes = params[:draw_method]
      @draw_method.owner = user
      @draw_method.path = "#{@directory}/#{@draw_method.name}"
      Node.make_user_directory(@directory, user, 0)
      option_num = params[:option_num].to_i
      option_num.times{|i|
        param = params[:option][i.to_s]
        value_type = param.delete(:value_type)
        dma = DrawMethodOption.new(param)
        dma.draw_method = @draw_method
        dma.value_type = ValueType.find(:first,:conditions=>["name=?",value_type])
        dma.save!
      }
      Node.make_user_directory(@directory, user, 0)
      groups = params[:groups]
      groups.shift if groups[0] == 'only me'
      @draw_method.other_mode = 0
      @draw_method.set_rgroups(groups) if groups.length > 0
      if check_erb(@draw_method) && check_script(@draw_method)
        if @draw_method.save
          flash[:notice] = "create successful"
          redirect_back_or_default :action => "list"
        else
          flash[:notice] = "create unsuccessful"
        end
      end
    when :get
      @draw_method = DrawMethod.new
    end
    @groups = user.own_groups + user.belonging_groups
    @user = user
  end

  def edit
    user = (login=session[:user]) && User.find_by_login(login)
    @draw_method = DrawMethod.find(:first, :conditions=>["name=?",params[:id]], :user=>user)
    unless @draw_method && @draw_method.owner==user
      redirect_back_or_default :action => "list"
      return
    end
    @directory = "/usr/#{user.login}/draw_methods"
    case request.method
    when :post
      @draw_method.attributes = params[:draw_method]
      option_num = params[:option_num].to_i
      dmas = @draw_method.draw_method_options
      option_num.times{|i|
        param = params[:option][i.to_s]
        value_type = param.delete(:value_type)
        if dmas[i]
          dma = dmas[i]
          dma.attributes = param
        else
          dma = dmas.create(param)
        end
        dma.value_type = ValueType.find(:first,:conditions=>["name=?",value_type])
        dma.save
      }
      for i in option_num...dmas.length
        dmas[i].destroy
      end
      groups = params[:groups]
      groups.shift if groups[0] == 'only me'
      @draw_method.other_mode = 0
      @draw_method.set_rgroups(groups) if groups.length > 0
      if check_erb(@draw_method) && check_script(@draw_method)
        if @draw_method.save
          flash[:notice] = "edit successful"
          redirect_back_or_default :action => "list"
        else
          flash[:notice] = "edit unsuccessful"
        end
      end
    end
    @groups = user.own_groups + user.belonging_groups
    @user = user
  end

  def list
    user = (login=session[:user]) && User.find_by_login(login)
    dms = DrawMethod.find(:all, :user => user)
    @own_dms = Array.new
    @other_dms = Array.new
    dms.each{|dm|
      if dm.owner == user
        @own_dms.push dm
      else
        @other_dms.push dm
      end
    }
    @super_user = user.super_user
 end

  def details
    name = params[:id]
    user = (login=session[:user]) && User.find_by_login(login)
    dm = DrawMethod.find(:first, :conditions=>["name=?",name], :user=>user)
    if dm
      @draw_method = dm
      @user = user
    else
      redirect_back_or_default :action => "list"
      return
    end
  end

  def create_setting_html
    unless request.xhr?
      render :nothing => true
      return
    end
    name = params[:name]
    nopts = params[:nopts].to_i
    opts = params[:option]
    html = ""
    if nopts > 0
      html << "<table>\n"
      nopts.times{|n|
        html << "  <tr>\n"
        opt = opts[n.to_s]
        html << <<-"EOS"
    <td>#{opt["name"]}</td>
    <td>(#{opt["value_type"]})</td>
    <td><input type="text" size="5" value="#{opt["default"]}" name="analysis[#{name}_#{opt["name"]}][#{n}]"/></td>
        EOS
        html << "  </tr>\n"
      }
      html << "</table>\n"
    end
    render :update do |page|
      page << "$('draw_method_setting_html').value='#{escape_javascript(html)}'"
    end
  end


  private

  def check_script(function)
    script = function.script
    ggraph = function.ggraph
    if script =~ /retry/ || ggraph =~ /retry/
      flash[:notice] = "you cannot use retry in script"
      return false
    end
    return true
  end

  def check_erb(draw_method)
    html = draw_method.setting_html
    begin
      ERB.new(html).run
      return true
    rescue SyntaxError
      flash[:notice] = "html is invalid"
      return false
    end
  end


end
