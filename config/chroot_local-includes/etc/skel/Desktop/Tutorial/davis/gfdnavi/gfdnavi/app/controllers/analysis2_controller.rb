#require "json"
require "virtual_data"

class Analysis2Controller < ApplicationController
  layout "gfdnavi"

  before_filter :login_required, :only => :save

  def index
    @user = (login=session[:user]) && User.find_by_login(login)
    @vars = session[:variables_list] || Array.new
    @diagrams = (session[:diagrams] || Array.new).compact
    @diagrams.unshift(params[:path]) if (params[:path])
    session[:diagrams] = @diagrams
    @diagrams.each{|path| add_vars_from_path(path)}
    @all_axes = @vars.map{|v| path_to_axes(v)}
    @uniq_axes = []
    @all_axes.each{|axes| axes.each{|ax| @uniq_axes.push([ax["name"], ax["units"]])}}
    @uniq_axes.uniq!
    @all_axes_index = @all_axes.map{|axes| axes.map{|ax| @uniq_axes.index([ax["name"], ax["units"]])}}
  end

  def get_axes
    raise unless params[:path]
    render :text => path_to_axes(params[:path]).to_json
    return
  end

  def get_diagrams
    @diagrams = (session[:diagrams] || Array.new).compact
    @dg_parsed = @diagrams.collect{|path| convert2layers(parse(path)).to_json}
    render :text => @dg_parsed.inspect # Array of JSON strings
  end

  def set_diagrams
    # params[:paths] is an Array of JSON strings
    session[:diagrams] = []
    ActiveSupport::JSON.decode(params[:paths]).each{|path|
      #session[:diagrams].push(NumRu::GfdnaviData.open(path).path)
      session[:diagrams].push(path)
    }
    render :nothing => true
    return
  end

  def clear
    session[:diagrams] = []
    session[:variables_list] = []
    render :nothing => true
    return
  end

  private 

  def parse(path)
    gd = NumRu::GfdnaviData.open(path)
    obj = gd.get_object
    result = Hash.new
    if obj.is_a?(VirtualData)
      result["input"] = obj.original_nodes.collect{|on|
        if on.is_a?(NumRu::GfdnaviData::Variable)
          input = parse(on.path)
        elsif on.is_a?(Variable)
          input = on.path
        else
          raise on.inspect
        end
        input
      }
      if obj.functions && obj.functions.length > 0
        #result["functions"] = obj.functions.collect{|fn|
        func = Hash.new
        obj.functions.each{|fn|
          case fn[:type]
          when :cut
            func["cut"] = Hash.new
            # Range object must be converted to String
            fn[:value].each{|k,v| func["cut"][k] = v.to_s}
          when :func
            raise unless /\A\/usr\/([^\/]+)\/functions\// =~ fn[:func].path
            user = $1
            func["func"] = fn[:func].name
            func["func"] += ("," << user) unless user == "root"
            func["args"] = fn[:args]
          when :index
            func["index"] = fn[:index]
          end
        }
        result["functions"] = func
      end
      if obj.draw_method
        raise unless /\A\/usr\/([^\/]+)\/draw_methods\// =~ obj.draw_method[0].path
        user = $1
        result["draw_method"] = obj.draw_method[0].name
        result["draw_method"] += ("," << user) unless user == "root"
        result["draw_opts"] = obj.draw_method[1]
        result["draw_slice"] = obj.draw_slice[0]
      end
      if obj.overlayed_plots && obj.overlayed_plots.length > 0
        result["overlay"] = []
        obj.overlayed_plots.each{|ovrly|
          result["overlay"].push(parse(ovrly.path))
        }
      end
    elsif obj.is_a?(Variable)
      result["input"] = [obj.path] # make an Array
    else
      raise obj.inspect
    end
    return result
  end

  def convert2layers(parsed_path)
    result = []
    if parsed_path["overlay"]
      result += parsed_path["overlay"]
      parsed_path.delete("overlay")
    end
    return result.unshift(parsed_path)
  end

  def path_to_axes(path)
    gd = NumRu::GfdnaviData.open(path, @user)
    obj = gd.get_object
    obj = VirtualData.new(obj) unless obj.respond_to?(:dimensions)
    dims = obj.dimensions.dup
    dims.length.times{|i| dims[i]['ary'] = dims[i]['ary'].to_a}
    return dims
  end

  def add_vars_from_path(path)
    gd = NumRu::GfdnaviData.open(path, @user)
    obj = gd.get_object
    if obj.is_a?(VirtualData)
      obj.original_nodes.each{|on|
        if on.is_a?(NumRu::GfdnaviData::Variable)
          add_vars_from_path(on.path)
        elsif on.is_a?(Variable)
          @vars.push(on.path).uniq!
        else
          raise on.inspect
        end
      }
      if obj.overlayed_plots && obj.overlayed_plots.length > 0
        obj.overlayed_plots.each{|ovrly|
          add_vars_from_path(ovrly.path)
        }
      end
    elsif obj.is_a?(Variable)
      @vars.push(obj.path).uniq!
    else
      raise obj.inspect
    end
  end
end
