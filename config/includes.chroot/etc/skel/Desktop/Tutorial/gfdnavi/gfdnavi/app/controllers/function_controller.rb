class FunctionController < ApplicationController
  layout "gfdnavi"

  before_filter :login_required, :except => [:index, :list, :details]

  def index
    redirect_to :action => "list"
  end

  def create
    user = (login=session[:user]) && User.find_by_login(login)
    @directory = "/usr/#{user.login}/functions"
    case request.method
    when :post
      @function = Function.new
      @function.attributes = params[:function]
      @function.owner = user
      @function.path = "#{@directory}/#{@function.name}"
      Node.make_user_directory(@directory, user, 0)
      groups = params[:groups]
      groups.shift if groups[0] == 'only me'
      if user.super_user? && groups[0] == 'everyone' && groups.shift
        @function.other_mode = 4
      else
        @function.other_mode = 0
      end
      @function.set_rgroups(groups) if groups.length > 0
      if check_erb(@function) && check_script(@function)
        flag = nil
        Function.transaction do
          if flag = @function.save
            output_num = params[:output_num].to_i
            output_num.times{|i|
              fo = FunctionOutput.new( params[:output][i.to_s] )
              fo.function = @function
              fo.save!
            }
            argument_num = params[:argument_num].to_i
            argument_num.times{|i|
              param = params[:argument][i.to_s]
              value_type = param.delete(:value_type)
              fa = FunctionArgument.new( param )
              fa.function = @function
              fa.value_type = ValueType.find(:first,:conditions=>["name=?",value_type])
              fa.save!
            }
            @function.save!
          end
        end
        if flag
          flash[:notice] = "create successful"
          redirect_back_or_default :action => "list"
        else
          flash[:notice] = "create unsuccessful"
        end
      end
    when :get
      @function = Function.new
    end
    @groups = user.own_groups + user.belonging_groups
    @user = user
  end

  def edit
    user = (login=session[:user]) && User.find_by_login(login)
    @function = Function.find(:first, :conditions=>["name=?",params[:id]], :user=>user)
    unless @function && @function.owner==user
      redirect_back_or_default :action => "list"
      return
    end
    @directory = "/usr/#{user.login}/functions"
    case request.method
    when :post
      @function.attributes = params[:function]
      output_num = params[:output_num].to_i
      outputs = @function.function_outputs
      output_num.times{|i|
        param = params[:output][i.to_s]
        if outputs[i]
          fo = outputs[i]
          fo.attributes = param
        else
          fo = outputs.create(param)
        end
        fo.save
      }
      for i in output_num...outputs.length
        outputs[i].destroy
      end
      argument_num = params[:argument_num].to_i
      args = @function.function_arguments
      argument_num.times{|i|
        param = params[:argument][i.to_s]
        value_type = param.delete(:value_type)
        if args[i]
          fa = args[i]
          fa.attributes = param
        else
          fa = args.create(param)
        end
        fa.value_type = ValueType.find(:first,:conditions=>["name=?",value_type])
        fa.save
      }
      for i in argument_num...args.length
        args[i].destroy
      end
      groups = params[:groups]
      groups.shift if groups[0] == 'only me'
      if user.super_user? && groups[0] == 'everyone' && groups.shift
        @function.other_mode = 4
      else
        @function.other_mode = 0
      end
      @function.set_rgroups(groups) if groups.length > 0
      if check_erb(@function) && check_script(@function)
        if @function.save
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
    funcs = Function.find(:all, :user => user)
    @own_funcs = Array.new
    @other_funcs = Array.new
    funcs.each{|func|
      if func.owner == user
        @own_funcs.push func
      else
        @other_funcs.push func
      end
    }
    @super_user = user.super_user
 end

  def details
    name = params[:id]
    user = (login=session[:user]) && User.find_by_login(login)
    func = Function.find(:first, :conditions=>["name=?",name], :user=>user)
    if func
      @func = func
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
    narg = params[:narg].to_i
    args = params[:argument]
    html = ""
    if narg > 0
      html << "<table>\n"
      narg.times{|n|
        html << "  <tr>\n"
        arg = args[n.to_s]
        html << <<-"EOS"
    <td>#{arg["description"]}</td>
    <td>(#{arg["value_type"]})</td>
    <td><input type="text" size="5" value="#{arg["default"]}" name="analysis[function_arguments][#{n}]"/></td>
        EOS
        html << "  </tr>\n"
      }
      html << "</table>\n"
    end
    render :update do |page|
      page << "$('function_setting_html').value='#{escape_javascript(html)}'"
    end
  end

  private

  def check_script(function)
    script = function.script
    if script =~ /retry/
      flash[:notice] = "you cannot use retry in script"
      return false
    end
    return true
  end

  def check_erb(function)
    html = function.setting_html
    begin
      ERB.new(html).run
      return true
    rescue #SyntaxError
      flash[:notice] = "html is invalid"
      return false
    end
  end

end
