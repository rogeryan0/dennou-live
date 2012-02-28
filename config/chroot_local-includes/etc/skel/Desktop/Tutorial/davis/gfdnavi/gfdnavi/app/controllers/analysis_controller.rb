require "numru/vizshot_gfdnavi"
require "virtual_data"

class AnalysisController < ApplicationController
  layout "gfdnavi"

  before_filter :login_required, :only => :save

  def index
    if path = params[:path]
      analysis = Analysis.create_from_path(path)
      if analysis.draw_method
        session[:analysis] = analysis
        ary = (session[:variables_list] ||= Array.new)
        analysis.variables.each do |var|
          pa = var.path.sub(/\/cut\([^)]+\)\Z/,"")
          ary.push(pa) unless ary.include?(pa)
        end
        #      redirect_to
        execute_draw(analysis, false)
        return
      else
        @analysis = analysis
      end
    else
      @analysis = (session[:analysis] ||= Analysis.new)
    end
    @analysis.user = (login=session[:user]) && User.find_by_login(login)
    @variables = all_variables(@analysis)
    @draw_methods = draw_method_set(@analysis)
    @functions = function_set(@analysis)
    @history = session[:history]
    @diagrams = session[:diagrams]
  end

  def load_history
    i = params[:id].to_i
    unless session[:history][i]
      render :update do |page|
          page.replace_html :messages, "operation failed. please reload the page."
      end
    end
    analysis = session[:history][i][0]
    session[:analysis] = analysis
    redirect_to(:action => "index")
  end


  def clear
    session[:analysis] = nil
    session[:history] = nil
    session[:diagrams] = nil
    clear_variables
    redirect_to(:action => "index")
  end

  def clear_selected
    var_list = session[:variables_list]
    var_selected = session[:analysis].variables.collect{|v| v.path}
    session[:analysis] = nil
    session[:variables_list] = var_list - var_selected
    redirect_to(:action => "index")
  end

  def clear_diagrams
    session[:diagrams] = nil
    if request.xhr?
      render(:update){|page|
        page.replace_html :diagrams, ""
      }
    else
      redirect_to(:action => "index")
    end
  end

  def variables_selected
    unless request.xhr?
      clear
      return
    end
    session[:analysis] = variables_set(params["variables"])
    action_type_selected
  end

  def action_type_selected
    unless request.xhr?
      clear
      return
    end
    user = (login=session[:user]) && User.find_by_login(login)
    @analysis = session[:analysis] || Analysis.new
    @analysis.user = session[:user]
    @analysis.action_type = params[:action_type] if params[:action_type]
    if Analysis::ACTION_TYPE[@analysis.action_type] == "draw"
      unless @analysis.draw_method
        dm = nil
        if @analysis.dimensions.length >= 2
          dm = DrawMethod.find(:first,:conditions=>["name=?","tone_contour"],:user=>user)
          dm ||= DrawMethod.find(:first,:conditions=>"nvars>1",:user=>user)
        end
        dm ||= DrawMethod.find(:first,:conditions=>"nvars=1",:user=>user)
        unless dm
          raise "no draw method"
        end
        @analysis.draw_method = dm
      end
      @draw_methods = draw_method_set(@analysis)
    elsif Analysis::ACTION_TYPE[@analysis.action_type] == "analysis"
      @functions = function_set(@analysis)
    end

    session[:analysis] = @analysis
    messages = ""
    if @analysis.variables.length == 0
      @analysis.variable_clear
    elsif @analysis.dimensions.length == 0
      messages = "all selected variables must have the same dimensions"
    end

    render :update do |page|
      page.replace_html :messages, messages
      page.replace_html :dimensions_setting, render(:partial => "dimension_option")
      case Analysis::ACTION_TYPE[@analysis.action_type]
      when "draw"
        page.replace_html :draw_settings, render(:partial => "draw_settings")
      when "analysis"
        page.replace_html :analysis_settings, render(:partial => "analysis_settings")
#        page.replace_html :popular_diagrams, ""
      end

      page.replace_html :script, <<"EOF"
<script>
  action_type = #{@analysis.action_type};
  executable = #{@analysis.dimensions.length > 0};
  afterCallBack();
</script>
EOF
    end

  end

  def execute
    params_analysis = params[:analysis] || Hash.new
    if params_analysis[:draw_anim]=="1" && (anim = params[:anim])
      val = anim["val"]
      axes_org = params_analysis["axes"].dup
      anim_dim = params_analysis["draw_anim_dim"]
      params_analysis["axes"].delete(anim_dim)
      params_analysis["axes"][anim_dim] = {"min"=> val}
      anim = true
    else
      anim = false
    end
    action_type = params[:action_type]
    if request.xhr?
      an = session[:analysis]
      unless an
        render :update do |page|
          page.replace_html :messages, "operation failed. please reload the page."
        end
        return
      end
      analysis = Analysis.new
      analysis.variables = an.variables
      analysis.draw_keep = an.draw_keep
      analysis.draw_share = an.draw_share
    else
      analysis = variables_set(params["variables"])
      analysis.draw_keep = false
      analysis.draw_share = false
    end
    analysis.user = (login=session[:user]) && User.find_by_login(login)
    analysis.draw_anim = anim
    analysis.action_type = Analysis::ACTION_TYPE.index(action_type)
    analysis.attributes = params_analysis
    session[:analysis] = analysis
    res = false
    case action_type
    when "draw"
      res = execute_draw(analysis, anim)
      if anim
        analysis.axes = axes_org
        session[:analysis] = analysis
      end
      return
    when "analysis"
      if request.xhr?
        res = execute_analysis(analysis)
        return
      end
    end
    if res
      session[:history] ||= AnalysisHistory.new
      session[:history].push YAML.load(analysis.to_yaml)
    else
      raise "invalid action (#{action_type})"
    end
  end

  def download_diagram_script_and_data
    id = params[:id]
    diagram = get_temp_diagram(id)
    if diagram
      viz = YAML.load(diagram.path)
      if NumRu::VizShotGfdnavi === viz
        flag = true
        viz.get_variables.each{|v|
          unless v.downloadable?
            flag = false
            break
          end
        }
        if flag
          res = viz_to_script_and_data(viz, work_dir)
          if res[0]
            send_file res[1], :filename => "gfdnavi.tar.gz"
            return
          else
            message = res[1]
          end
        else
          message = "download is inhivited"
        end
      else
        message = "cannot find data to download"
      end
    else
      message = "cannot find diagram"
    end
    flash[:messages] = "failed to download<br/>" + message
    redirect_to :action => "index"
  end

=begin
  def upload_file
    user = (login=session[:user]) && User.find_by_login(login)
    if user
      file = params[:file]
      unless path == ""
        path = File.join(work_dir,File.basename(file.path))
        File.move(file.path, path)
        session[:temp_variables_list] ||= Array.new
        NumRu::GPhys::IO.var_names_except_coordinates(path).each{|vname|
          gphys = NumRu::GPhys::IO.open(path,vname)
          var = Variable.new
          var.file = "temporary:#{path.sub(/^#{GFDNAVI_WORK_PATH}/,"")}"
          var.path = File.join(var.file, vname)
          var.name = vname
          var.mtime = Time.now
          var.owner = user
          var.size = gphys.length
          node = var.node
          gphys.att_names.each{|an|
            val = gphys.get_att(an)
            val = NArray[val] if Numeric === val
            ka = KeywordAttribute.new
            ka.name = Kconv.kconv(an,Kconv::UTF8)
            ka.value = val
            ka.node = node
            node.keyword_attributes.push(ka)
          }
          session[:temp_variables_list].push var
        }
      end
    end
    redirect_to(:action => "index")
  end
=end

  def download_data
    path = params[:path]
    var = get_temp_variable(path)
    if var
      if var.downloadable?
        fname = var.fname
        if fname
          send_file fname, :filename => "gfdnavi.nc"
          return
        end
      else
        mess = "download is inhivited"
      end
    else
      mess = "cannot download data"
    end
    render :update do |page|
      page.replace_html :messages, mess
    end
  end

=begin
  def save
    user = (login=session[:user]) && User.find_by_login(login)
    unless user
      redirect_to :action => "index"
      return
    end
    
    case params[:path]
    when /\.\.\//
      flash[:notice] = '"../" cannot be used in path.'
      redirect_to :action => "save"
      return
    when /^\s*$/
      flash[:notice] = 'Path cannot be empty.'
      redirect_to :action => "save"
      return
    when /^\s*\//, /\/\s*$/, /\/\s*\//
      flash[:notice] = "Directory that doesn't have name is forbidden."
      redirect_to :action => "save"
      return
    end
    
    if request.get?
      @id = params[:id]
      @type = params[:type]
      @groups = user.belonging_groups
      @user = user
      case @type
      when "diagram"
        @suffix = "png"
      when "data"
        @name = get_temp_variable(@id).name
        @suffix = "nc"
      end
      render
      return
    end
    id = params[:id]
    type = params[:type]
#    name = params[:name]
    path = params[:path]
    description = params[:description]
    keywords = params[:keywords]
    groups = params[:groups].values # Hash to Array

    if groups[0] == 'everyone'
      other_mode = 4
      groups.shift
    else
      other_mode = 0
    end
    refs = Array.new
    case type
    when "diagram"
      var = Image.new
      diagram = get_temp_diagram(id)
      var.org_path = diagram.path
      from = diagram.path
      name = var.name = File.basename(path)
      var.path = File.join("/usr",Knowledge.remove_scheme(user),path)
    when "data"
      var = get_temp_variable(id)
      from = var.fname
      name = var.name
      path = "/#{path}.nc/#{name}"
      var.path = File.join("/usr",Knowledge.remove_scheme(user),path)
      var.file = File.dirname(var.path)
    end
    var.owner = user
    var.other_mode = other_mode
    var.node.set_rgroups(groups) if groups.length > 0

    var.keyword_attributes.build(:name => "description", :value => description) unless description == ""
    keywords.each{|k,v|
      if Hash === v && (name=v["name"])!="" && (val=v["value"])!=""
        if val.to_i.to_s == val
          val = NArray[val.to_i]
        elsif val.to_f.to_s == val
          val = NArray[val.to_f]
        end
        var.keyword_attributes.build(:name => name, :value => val)
      end
    }

    to = var.fname
    File.makedirs( File.dirname(to) )
    full_path = ""
    parent = nil
    var.path.split(File::Separator)[0..-2].each{|dname|
      full_path = File.join(full_path, dname)
      dir = Directory.find(:first, :conditions=>["path=?",full_path], :user=>user)
      unless dir
        dir = Directory.new
        dir.name = dname
        dir.path = full_path
        dir.parent = parent.node
        dir.owner = user
        dir.other_mode = other_mode
        dir.set_rgroups(groups) if groups.length > 0
        if full_path == var.file
          dir.downloadable = var.downloadable?
          dir.plain_file = true
        end
        dir.save!
      end
      parent = dir
    }

    if var.save
      rtype = {"diagram"=>"draw", "data"=>"analyze"}[type]
      refs.each{|ref|
        NodeRelation.new(:name=>rtype, :reference=>ref, :referenced_by=>var.node).save!
      }
      messages = "successfully saved"
      case type
      when "diagram"
        File.copy( from, to )
      when "data"
        raise("[BUG] from == to") if from==to
        File.move( from, to )
        session[:variables_list] ||= Array.new
        session[:variables_list].push var.path
      end
    else
      messages = "failed to save<br/>"
      messages += var.errors.full_messages.join("<br/>") if var.errors
    end
    flash[:messages] = messages
    redirect_to :action => :index
  end
=end

  def delete_diagram
    id = params[:id].to_i
    if (diagram = session[:diagrams][id])
      session[:diagrams][id] = nil
    end
    render :nothing => true
  end

  def pile_up
    unless request.xhr?
      render :nothing => true
      return
    end
    lower_id = params[:lower].to_i
    upper_id = params[:upper].to_i
    if (diagrams = session[:diagrams])
      newpath = "/[" + diagrams[lower_id] + "," +diagrams[upper_id] + "]/overlay()"
      id = diagrams.length
      diagrams.push(newpath)
      render(:update){|page|
        page.replace_html :messages, ""

        @diagram = [id, newpath]
        page.insert_html :top, :diagrams, render(:partial => "diagram")

        # "Create Knowledge from Analysis" button is disabled if login menu is disabled.
        page << "drawKnowledgeFromAnalysisButton();" unless GFDNAVI_DISABLE_USER && session[:user].nil?

        @history = session[:history]
        page.replace_html :history, render(:partial => "history")
      }
    end
    return
  end

  def preview
    unless request.xhr?
      render :nothing => true
      return
    end
    h = params[:function] || params[:draw_method]
    @html = ERB.new(h[:setting_html]||"").result
    @analysis = Analysis.new
    render :layout => false
  end

  def show_image
    id = params[:id]
    if (dc = DiagramCache.find(id))
      response.headers['Content-Type'] = 'image/png'
      response.headers['Pragma'] = 'no-cache'
      File.open(dc.files[0],"rb"){|file|
        render :text => file.read
      }
    else
      render :nothing => true
    end
  end
  
  # end
  private
  def all_variables(analysis)
    selected_vars = analysis.variables.collect{|v| v.path}
    (user = analysis.user) && user = user.login
    vars = (session[:variables_list] ||= Array.new)

    variables = Array.new

    vars.each do |path|
      if gd = NumRu::GfdnaviData.open(path, user)
        #unless path == gd.path
        #  raise "BUG: #{path} =! #{gd.path}"
        #end
        # !!! ORDER OF OPTIONS CAN BE CHANGED BETWEEN path AND gd.path !!!
        variables.push [gd.name, gd.path, selected_vars.include?(path)]
      end
    end
    return variables
  end

  def get_variables(analysis, opts = {})
    vars = analysis.variables
    if vars.length == 1 && NumRu::GfdnaviData === vars[0]
      var = vars[0]
    else
      if (order = opts[:order]) && order.is_a?(Array) && order.any?
        nvars = vars.length
        vars_new = Array.new(nvars)
        order.flatten!
        nvars.times do |i|
          vars_new[i] = vars[order[i]]
        end
        vars = vars_new
      end
      var = NumRu::GfdnaviData::Array[ *vars ]
    end
    if opts[:cut]
      cut = Hash.new
      analysis.axes.each do |name, hash|
        if (!hash["max"]) || (hash["min"].to_f == hash["max"].to_f)
          cut[name] = hash["min"].to_f
        else
          cut[name] = (hash["min"].to_f)..(hash["max"].to_f)
        end
      end
    end
    var = var.cut(cut)
    return var
  end


  def draw_method_set(analysis)
    dms = Array.new
    selected_vars = analysis.variables
    nvars = selected_vars.length
    ndims = analysis.common_dimensions.length
    user = (login=session[:user]) && User.find_by_login(login)
    nvars.times{|i|
      if nvars%(i+1) == 0
        dms += DrawMethod.find(:all, :conditions => ["ndims<=? and nvars=?", ndims, i+1], :user => user)
      end
    }
    h = Hash.new{|h,k| k}    # default value == key
    h["tone_contour"] = "0"  # highest precedence in sorting
    dms.sort!{|a,b| h[a.name] <=> h[b.name]}
    return dms
  end

  def function_set(analysis)
    funcs = Array.new
    nvars = analysis.variables.length
    user = (login=session[:user]) && User.find_by_login(login)
    nvars.times{|i|
      if nvars%(i+1) == 0
        funcs += Function.find(:all, :conditions => ["nvars=?", i+1], :user => user)
      end
    }
    return funcs
  end

#draw
  def execute_draw(analysis, anim=false)
    keep = anim || analysis.draw_keep
    dm = analysis.draw_method
    dm_opts = analysis.get_draw_options
    vorder = dm_opts.delete("variables_order")
    if vorder.is_a?(Hash)
      tmpvorder = []
      vorder.each{|key, val| tmpvorder[key.to_i] = val}
      vorder = tmpvorder
    end
    vars = get_variables(analysis, :order => vorder, :cut => true)
    plots = vars.plot(dm, dm_opts)
    if keep
      dgs = (session[:diagrams] ||= Array.new)
    else
      dgs = (session[:diagrams] = Array.new)
    end
    id0 = dgs.length
    new_dgs = Array.new
    plots.each{|plot|
      path = plot.path
      dgs.push path
      new_dgs.push path
      # make the actual image file(s) and the cache on the database
      NumRu::GfdnaviData::Local.parse_path(path,analysis.user).to_png
    }
    if request.xhr?
      @analysis = session[:analysis]
      render(:update){|page|
        page.replace_html :messages, ""
        @anim = anim
        @pile = @analysis.draw_pileup
        new_dgs.each_with_index{|path,i|
          id = id0 + i
          @diagram = [id, path]
          if keep || i>0
            page.insert_html :top, :diagrams, render(:partial => "diagram")
          else
            page.replace_html :diagrams, render(:partial => "diagram")
          end
          if anim
            id_html = "diagram_#{id}"
            page << "anim.diagrams.push($('#{id_html}_table'));"
            page << "anim.next();"
          end
        }
        # "Create Knowledge from Analysis" button is disabled if login menu is disabled.
        page << "drawKnowledgeFromAnalysisButton();" unless GFDNAVI_DISABLE_USER && session[:user].nil?

        @history = session[:history]
        page.replace_html :history, render(:partial => "history")
      }
      return
    else
      @diagrams = dgs#diagrams
      redirect_to
#      render :action => :show_diagram
    end
  end

# analysis
  def execute_analysis(analysis)
    func = analysis.function
    args = analysis.function_arguments
    vorder = analysis.function_variables_order
#    vorder = nil
    vars = get_variables(analysis, :order => vorder, :cut => true)
    vars = vars.analysis(func, *args)
    if vars
      vs = (session[:variables_list] ||= Array.new)
      vars.each do |var|
        path = var.path
        vs.push path unless vs.include?(path)
      end
      @analysis = analysis
      user = (login=session[:user]) && User.find_by_login(login)
      @variables = all_variables(@analysis)
      render :update do |page|
#        page.replace_html :messages, messages
        page.replace_html :variables_body, render(:partial => "variables")
        if GFDNAVI_BENCHMARK
          html = host_information_table
          html << <<-"EOS"
            Number of created variable is #{vars.length}.<br/>
          EOS
          page << "benchMark.set(null, '#{escape_javascript(html)}');"
          page << "benchMark.complete();"
        end
        @history = session[:history]
        page.replace_html :history, render(:partial => "history")
      end
      return true
    else
      render :update do |page|
        page.replace_html :messages, messages
      end
      return false
    end
  end


#common
  def get_temp_variable(path)
    variables = session[:temp_variables_list]
    if variables
      /^temp_(.*)/ =~ path
      id = $1.to_i
      return variables[id]
    else
      return nil
    end
  end

  def get_temp_diagram(id)
    diagrams = session[:diagrams]
    if diagrams
      id = id.to_i
      return DiagramCache.find(diagrams[id])
    else
      return nil
    end
  end

  def clear_variables
    session[:variables_list] = nil
  end

  def auto_file_path(type)
    case type
    when "data"
      suffix = "nc"
    when "diagram"
      suffix = "png"
    end
    path = user_path+"/auto"
    FileUtils.makedirs(path) unless File.exist?(path)
    last = Dir["#{path}/gfdnavi_\d+\.#{suffix}"][0]
    if last
      last =~ /gfdnavi_(\d+)\./
      num = $1.next
    else
      num = "0000"
    end
    fname = "gfdnavi_#{num}.#{suffix}"
    full_path = "#{path}/#{fname}"
    path = "/usr/#{session[:user]}/auto/#{fname}"
    return [full_path, path]
  end

  def variables_set(params_vars)
    user = session[:user]
    analysis = session[:analysis] || Analysis.new
    analysis.user = user
    analysis.variable_clear
    params_vars && params_vars.each do |k,v|
      unless v == "0"
        if (var = NumRu::GfdnaviData::Local.parse_path(URI.unescape(k), user))
          analysis.variables.push var unless analysis.variables.include?(var)
        end
      end
    end
    if ( var = analysis.variables[0] ) && Variable === (var = var.get_object)
      ans = [var]
      pa = var
      while (pa = pa.parent)
        ans.unshift pa
      end
      ans.each do |v|
        v.draw_parameters.each{|dp|
          analysis[dp.name] = dp.value
        }
      end
    end
    return analysis
  end


end
